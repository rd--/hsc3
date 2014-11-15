--  | Unit Generator ('UGen'), and associated types and instances.
module Sound.SC3.UGen.Type where

import Data.Bits {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Safe {- safe -}
import System.Random {- random -}
import qualified Text.Read as R {- base -}

import Sound.SC3.UGen.MCE
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate

-- * Basic types

-- | Data type for internalised identifier at 'UGen'.
data UGenId = NoId | UId Int
              deriving (Eq,Show)

-- | Alias of 'NoId', the 'UGenId' used for deterministic UGens.
no_id :: UGenId
no_id = NoId

-- | SC3 samples are 32-bit 'Float'.  hsc3 represents data as 64-bit
-- 'Double'.  If 'UGen' values are used more generally (ie. see
-- hsc3-forth) 'Float' may be too imprecise, ie. for representing time
-- stamps.
type Sample = Double

-- | Constants.
--
-- > Constant 3 == Constant 3
-- > (Constant 3 > Constant 1) == True
data Constant = Constant {constantValue :: Sample}
                deriving (Eq,Ord,Show)

-- | Control meta-data.
data C_Meta n =
    C_Meta {ctl_min :: n -- ^ Minimum
           ,ctl_max :: n -- ^ Maximum
           ,ctl_warp :: String -- ^ @(0,1)@ @(min,max)@ transfer function.
           ,ctl_step :: n -- ^ The step to increment & decrement by.
           ,ctl_units :: String -- ^ Unit of measure (ie hz, ms etc.).
           }
    deriving (Eq,Show)

-- | 5-tuple form of 'C_Meta' data.
type C_Meta' n = (n,n,String,n,String)

-- | Lift 'C_Meta'' to 'C_Meta' allowing type coercion.
c_meta' :: (n -> m) -> C_Meta' n -> C_Meta m
c_meta' f (l,r,w,stp,u) = C_Meta (f l) (f r) w (f stp) u

-- | Control inputs.  It is an invariant that controls with equal
-- names within a UGen graph must be equal in all other respects.
data Control = Control {controlOperatingRate :: Rate
                       ,controlIndex :: Maybe Int
                       ,controlName :: String
                       ,controlDefault :: Sample
                       ,controlTriggered :: Bool
                       ,controlMeta :: Maybe (C_Meta Sample)}
               deriving (Eq,Show)

-- | Labels.
data Label = Label {ugenLabel :: String}
             deriving (Eq,Show)

-- | Unit generator output descriptor.
type Output = Rate

-- | Operating mode of unary and binary operators.
newtype Special = Special Int
    deriving (Eq, Show)

-- | UGen primitives.
data Primitive = Primitive {ugenRate :: Rate
                           ,ugenName :: String
                           ,ugenInputs :: [UGen]
                           ,ugenOutputs :: [Output]
                           ,ugenSpecial :: Special
                           ,ugenId :: UGenId}
                 deriving (Eq,Show)

-- | Proxy to multiple channel input.
data Proxy = Proxy {proxySource :: Primitive
                   ,proxyIndex :: Int}
            deriving (Eq,Show)

-- | Multiple root graph.
data MRG = MRG {mrgLeft :: UGen
               ,mrgRight :: UGen}
           deriving (Eq,Show)

-- | Union type of Unit Generator forms.
data UGen = Constant_U Constant
          | Control_U Control
          | Label_U Label
          | Primitive_U Primitive
          | Proxy_U Proxy
          | MCE_U (MCE UGen)
          | MRG_U MRG
            deriving (Eq,Show)

-- * Parser

parse_constant :: String -> Maybe UGen
parse_constant s =
    let d :: Maybe Double
        d = R.readMaybe s
    in fmap constant d

-- * Accessors

-- | See into 'Constant_U'.
un_constant :: UGen -> Maybe Constant
un_constant u =
    case u of
      Constant_U c -> Just c
      _ -> Nothing

-- | Value of 'Constant_U' 'Constant'.
u_constant :: UGen -> Maybe Sample
u_constant = fmap constantValue . un_constant

-- * Predicates

-- | Constant node predicate.
isConstant :: UGen -> Bool
isConstant = isJust . un_constant

-- | See into 'MRG_U', follows leftmost rule until arriving at non-MRG
-- node.
mrg_leftmost :: UGen -> UGen
mrg_leftmost u =
    case u of
      MRG_U m -> mrg_leftmost (mrgLeft m)
      _ -> u

-- | True if input is a sink 'UGen', ie. has no outputs.  Sees into
-- MRG.
isSink :: UGen -> Bool
isSink u =
    case mrg_leftmost u of
      Primitive_U p -> null (ugenOutputs p)
      MCE_U m -> all isSink (mceProxies m)
      _ -> False

-- | See into 'Proxy_U'.
un_proxy :: UGen -> Maybe Proxy
un_proxy u =
    case u of
      Proxy_U p -> Just p
      _ -> Nothing

isProxy :: UGen -> Bool
isProxy = isJust . un_proxy

-- * Validators

-- | Ensure input 'UGen' is valid, ie. not a sink.
checkInput :: UGen -> UGen
checkInput u =
    if isSink u
    then error ("checkInput: " ++ show u)
    else u

-- * Constructors

-- | Constant value node constructor.
constant :: Real n => n -> UGen
constant = Constant_U . Constant . realToFrac

-- | Type specialised 'constant'.
int_to_ugen :: Int -> UGen
int_to_ugen = constant

-- | Type specialised 'constant'.
float_to_ugen :: Float -> UGen
float_to_ugen = constant

-- | Type specialised 'constant'.
double_to_ugen :: Double -> UGen
double_to_ugen = constant

-- | Multiple channel expansion node constructor.
mce :: [UGen] -> UGen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> x
      _ -> MCE_U (MCE_Vector xs)

-- | Multiple root graph constructor.
mrg :: [UGen] -> UGen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> MRG_U (MRG x (mrg xs))

-- | Unit generator proxy node constructor.
proxy :: UGen -> Int -> UGen
proxy u n =
    case u of
      Primitive_U p -> Proxy_U (Proxy p n)
      _ -> error "proxy: not primitive"

-- * MCE

-- | Type specified 'mce_elem'.
mceProxies :: MCE UGen -> [UGen]
mceProxies = mce_elem

-- | Multiple channel expansion node ('MCE_U') predicate.  Sees into
-- MRG.
isMCE :: UGen -> Bool
isMCE u =
    case mrg_leftmost u of
      MCE_U _ -> True
      _ -> False

-- | Output channels of UGen as a list.  If required, preserves the
-- RHS of and MRG node in channel 0.
mceChannels :: UGen -> [UGen]
mceChannels u =
    case u of
      MCE_U m -> mceProxies m
      MRG_U (MRG x y) -> let r:rs = mceChannels x in MRG_U (MRG r y) : rs
      _ -> [u]

-- | Number of channels to expand to.  This function sees into MRG,
-- and is defined only for MCE nodes.
mceDegree :: UGen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      MCE_U m -> Just (length (mceProxies m))
      _ -> Nothing

-- | Erroring variant.
mceDegree_err :: UGen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree

-- | Extend UGen to specified degree.  Follows "leftmost" rule for MRG nodes.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n u =
    case u of
      MCE_U m -> mceProxies (mce_extend n m)
      MRG_U (MRG x y) -> let (r:rs) = mceExtend n x
                         in MRG_U (MRG r y) : rs
      _ -> replicate n u

-- | Apply MCE transform to a list of inputs.
mceInputTransform :: [UGen] -> Maybe [[UGen]]
mceInputTransform i =
    if any isMCE i
    then let n = maximum (map mceDegree_err (filter isMCE i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

-- | Build a UGen after MCE transformation of inputs.
mceBuild :: ([UGen] -> UGen) -> [UGen] -> UGen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> MCE_U (MCE_Vector (map (mceBuild f) i'))

-- | True if MCE is an immediate proxy for a multiple-out Primitive.
mce_is_direct_proxy :: MCE UGen -> Bool
mce_is_direct_proxy m =
    case m of
      MCE_Unit _ -> False
      MCE_Vector v ->
          let p = map un_proxy v
              p' = catMaybes p
          in all isJust p &&
             length (nub (map proxySource p')) == 1 &&
             map proxyIndex p' `isPrefixOf` [0..]

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf u =
    case u of
      Constant_U _ -> IR
      Control_U c -> controlOperatingRate c
      Label_U _ -> IR
      Primitive_U p -> ugenRate p
      Proxy_U p -> ugenRate (proxySource p)
      MCE_U _ -> maximum (map rateOf (mceChannels u))
      MRG_U m -> rateOf (mrgLeft m)

-- | Apply proxy transformation if required.
proxify :: UGen -> UGen
proxify u =
    case u of
      MCE_U m -> mce (map proxify (mceProxies m))
      MRG_U m -> mrg [proxify (mrgLeft m), mrgRight m]
      Primitive_U p ->
          let o = ugenOutputs p
          in case o of
               _:_:_ -> mce (map (proxy u) [0..(length o - 1)])
               _ -> u
      Constant_U _ -> u
      _ -> error "proxify: illegal ugen"

-- | Construct proxied and multiple channel expanded UGen.
--
-- cf = constant function, rs = rate set, r = rate, nm = name, i =
-- inputs, o = outputs.
mkUGen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [UGen] -> Maybe UGen -> Int -> Special -> UGenId -> UGen
mkUGen cf rs r nm i i_mce o s z =
    let i' = maybe i ((i ++) . mceChannels) i_mce
        f h = let r' = either id (maximum . map (rateOf . (atNote ("mkUGen: " ++ nm) h))) r
                  o' = replicate o r'
                  u = Primitive_U (Primitive r' nm h o' s z)
              in if r' `elem` rs
                 then case cf of
                        Just cf' ->
                            if all isConstant h
                            then constant (cf' (mapMaybe u_constant h))
                            else u
                        Nothing -> u
                 else error ("mkUGen: rate restricted: " ++ show (r,rs,nm))
    in proxify (mceBuild f (map checkInput i'))

-- * Operators

-- | Operator UGen constructor.
mkOperator :: ([Sample] -> Sample) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUGen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId

-- | Unary math constructor with constant optimization.
mkUnaryOperator :: Unary -> (Sample -> Sample) -> UGen -> UGen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimization.
--
-- > let o = sinOsc AR 440 0
--
-- > o * 1 == o && 1 * o == o && o * 2 /= o
-- > o + 0 == o && 0 + o == o && o + 1 /= o
-- > o - 0 == o && 0 - o /= o
-- > o / 1 == o && 1 / o /= o
-- > o ** 1 == o && o ** 2 /= o
mkBinaryOperator_optimize :: Binary -> (Sample -> Sample -> Sample) ->
                             (Either Sample Sample -> Bool) ->
                             UGen -> UGen -> UGen
mkBinaryOperator_optimize i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (Constant_U (Constant a'),_) ->
                 if o (Left a') then Just b else Nothing
             (_,Constant_U (Constant b')) ->
                 if o (Right b') then Just a else Nothing
             _ -> Nothing
   in fromMaybe (mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)) r

-- | Binary math constructor with constant optimization.
mkBinaryOperator :: Binary -> (Sample -> Sample -> Sample) ->
                    UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

-- * Numeric instances

-- | Unit generators are numbers.
instance Num UGen where
    negate = mkUnaryOperator Neg negate
    (+) = mkBinaryOperator_optimize Add (+) (`elem` [Left 0,Right 0])
    (-) = mkBinaryOperator_optimize Sub (-) (Right 0 ==)
    (*) = mkBinaryOperator_optimize Mul (*) (`elem` [Left 1,Right 1])
    abs = mkUnaryOperator Abs abs
    signum = mkUnaryOperator Sign signum
    fromInteger = Constant_U . Constant . fromInteger

-- | Unit generators are fractional.
instance Fractional UGen where
    recip = mkUnaryOperator Recip recip
    (/) = mkBinaryOperator_optimize FDiv (/) (Right 1 ==)
    fromRational = Constant_U . Constant . fromRational

-- | Unit generators are floating point.
instance Floating UGen where
    pi = Constant_U (Constant pi)
    exp = mkUnaryOperator Exp exp
    log = mkUnaryOperator Log log
    sqrt = mkUnaryOperator Sqrt sqrt
    (**) = mkBinaryOperator_optimize Pow (**) (Right 1 ==)
    logBase a b = log b / log a
    sin = mkUnaryOperator Sin sin
    cos = mkUnaryOperator Cos cos
    tan = mkUnaryOperator Tan tan
    asin = mkUnaryOperator ArcSin asin
    acos = mkUnaryOperator ArcCos acos
    atan = mkUnaryOperator ArcTan atan
    sinh = mkUnaryOperator SinH sinh
    cosh = mkUnaryOperator CosH cosh
    tanh = mkUnaryOperator TanH tanh
    asinh x = log (sqrt (x*x+1) + x)
    acosh x = log (sqrt (x*x-1) + x)
    atanh x = (log (1+x) - log (1-x)) / 2

-- | Unit generators are real.
instance Real UGen where
    toRational (Constant_U (Constant n)) = toRational n
    toRational _ = error "UGen.toRational: non-constant"

-- | Unit generators are integral.
instance Integral UGen where
    quot = mkBinaryOperator IDiv (error "UGen.quot")
    rem = mkBinaryOperator Mod (error "UGen.rem")
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator IDiv (error "UGen.div")
    mod = mkBinaryOperator Mod (error "UGen.mod")
    toInteger (Constant_U (Constant n)) = floor n
    toInteger _ = error "UGen.toInteger: non-constant"

instance RealFrac UGen where
  properFraction = error "UGen.properFraction, see properFractionE"
  round = error "UGen.round, see roundE"
  ceiling = error "UGen.ceiling, see ceilingE"
  floor = error "UGen.floor, see floorE"

-- | Unit generators are orderable (when 'Constants').
--
-- > (constant 2 > constant 1) == True
instance Ord UGen where
    (Constant_U a) < (Constant_U b) = a < b
    _ < _ = error "UGen.<, see <*"
    (Constant_U a) <= (Constant_U b) = a <= b
    _ <= _ = error "UGen.<= at, see <=*"
    (Constant_U a) > (Constant_U b) = a > b
    _ > _ = error "UGen.>, see >*"
    (Constant_U a) >= (Constant_U b) = a >= b
    _ >= _ = error "UGen.>=, see >=*"
    min = mkBinaryOperator Min min
    max = mkBinaryOperator Max max

-- | Unit generators are enumerable.
instance Enum UGen where
    succ u = u + 1
    pred u = u - 1
    toEnum n = Constant_U (Constant (fromIntegral n))
    fromEnum (Constant_U (Constant n)) = truncate n
    fromEnum _ = error "UGen.fromEnum: non-constant"
    enumFrom = iterate (+1)
    enumFromThen n m = iterate (+(m-n)) n
    enumFromTo n m = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m =
        let p = if n' >= n then (>=) else (<=)
        in takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')

-- | Unit generators are stochastic.
instance Random UGen where
    randomR (Constant_U (Constant l),Constant_U (Constant r)) g =
        let (n, g') = randomR (l,r) g
        in (Constant_U (Constant n), g')
    randomR _ _ = error "UGen.randomR: non constant (l,r)"
    random = randomR (-1.0, 1.0)

-- | UGens are bit patterns.
instance Bits UGen where
    (.&.) = mkBinaryOperator BitAnd undefined
    (.|.) = mkBinaryOperator BitOr undefined
    xor = mkBinaryOperator BitXor undefined
    complement = mkUnaryOperator BitNot undefined
    shift = error "UGen.shift"
    rotate = error "UGen.rotate"
    bitSize = error "UGen.bitSize"
    bit = error "UGen.bit"
    testBit = error "UGen.testBit"
    popCount = error "UGen.popCount"
    bitSizeMaybe = error "UGen.bitSizeMaybe"
    isSigned _ = True

{-
import Sound.SC3.UGen.Identifier

-- * UGen ID Instance

-- | Hash function for unit generators.
hashUGen :: UGen -> Int
hashUGen = hash . show

instance ID UGen where
    resolveID = hashUGen
-}
