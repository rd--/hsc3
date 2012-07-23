-- | UGen data structure representation and associated functions.
module Sound.SC3.UGen.UGen where

import Data.Bits
import qualified Data.Char as C
import Data.List
import Data.Maybe
import Sound.SC3.Identifier
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import System.Random {- random -}

-- * UGen Id type and functions

-- | Data type for internalised identifier at 'UGen'.
data UGenId = NoId | UId Int
              deriving (Eq,Show)

-- * Unit Generator type

-- | Unit generator.
data UGen = Constant { constantValue :: Double }
          | Control { controlOperatingRate :: Rate
                    , controlName :: String
                    , controlDefault :: Double
                    , controlTriggered :: Bool }
          | Label { ugenLabel :: String }
          | Primitive { ugenRate :: Rate
                      , ugenName :: String
                      , ugenInputs :: [UGen]
                      , ugenOutputs :: [Output]
                      , ugenSpecial :: Special
                      , ugenId :: UGenId }
          | Proxy { proxySource :: UGen
                  , proxyIndex :: Int }
          | MCE { mceProxies :: [UGen] }
          | MRG { mrgLeft :: UGen
                , mrgRight :: UGen }
            deriving (Eq, Show)

-- | Lookup operator name for operator UGens, else UGen name.
ugen_user_name :: String -> Special -> String
ugen_user_name nm (Special n) =
    case nm of
      "UnaryOpUGen" -> unaryName n
      "BinaryOpUGen" -> binaryName n
      _ -> nm

-- * UGen graph functions

-- | Depth first traversal of graph at `u' applying `f' to each node.
ugenTraverse :: (UGen -> UGen) -> UGen -> UGen
ugenTraverse f u =
    let rec = ugenTraverse f
    in case u of
         Primitive _ _ i _ _ _ -> f (u {ugenInputs = map rec i})
         Proxy s _ -> f (u {proxySource = rec s})
         MCE p -> f (u {mceProxies = map rec p})
         MRG l r -> f (MRG (rec l) (rec r))
         _ -> f u

-- | Right fold of UGen graph.
ugenFoldr :: (UGen -> a -> a) -> a -> UGen -> a
ugenFoldr f st u =
    let rec = flip (ugenFoldr f)
    in case u of
         Primitive _ _ i _ _ _ -> f u (foldr rec st i)
         Proxy s _ -> f u (f s st)
         MCE p -> f u (foldr rec st p)
         MRG l r -> f u (f l (f r st))
         _ -> f u st

-- * UGen ID Instance

-- | Hash function for unit generators.
hashUGen :: UGen -> Int
hashUGen = hash . show

instance ID UGen where
    resolveID = hashUGen

-- | Unit generator output descriptor.
type Output = Rate

-- | Operating mode of unary and binary operators.
newtype Special = Special Int
    deriving (Eq, Show)

-- * Unit generator node constructors

-- | Constant value node constructor.
constant :: (Real a) => a -> UGen
constant = Constant . realToFrac

-- | Control input node constructor.
--
--   Note that if the name begins with a t_ prefix the control is
--   not converted to a triggered control.  Please see tr_control.
control :: Rate -> String -> Double -> UGen
control r n d = Control r n d False

-- | Triggered (kr) control input node constructor.
tr_control :: String -> Double -> UGen
tr_control n d = Control KR n d True

-- | Multiple channel expansion node constructor.
mce :: [UGen] -> UGen
mce xs =
    case xs of
      [] -> error "mce: empty list"
      [x] -> x
      _ -> MCE xs

-- | Multiple root graph node constructor.
mrg2 :: UGen -> UGen -> UGen
mrg2 = MRG

-- | Unit generator proxy node constructor.
proxy :: UGen -> Int -> UGen
proxy = Proxy

-- * Unit generator node predicates

-- | Enumeration of 'UGen' types.
data UGenType = Constant_U
              | Control_U
              | Label_U
              | Primitive_U
              | Proxy_U
              | MCE_U
              | MRG_U
                deriving (Eq,Enum,Bounded,Show)

-- | Multiple channel expansion node predicate.
isMCE :: UGen -> Bool
isMCE = (== MCE_U) . ugenType

-- | Constant node predicate.
isConstant :: UGen -> Bool
isConstant = (== Constant_U) . ugenType

-- | Constant node predicate.
ugenType :: UGen -> UGenType
ugenType u =
    case u of
      Constant _ -> Constant_U
      Control _ _ _ _ -> Control_U
      Label _ -> Label_U
      Primitive _ _ _ _ _ _ -> Primitive_U
      Proxy _ _ -> Proxy_U
      MCE _ -> MCE_U
      MRG _ _ -> MRG_U

-- * Multiple channel expansion

-- | Multiple channel expansion for two inputs.
mce2 :: UGen -> UGen -> UGen
mce2 x y = mce [x, y]

-- | Extract two channels from possible MCE.
mce2c :: UGen -> (UGen,UGen)
mce2c u =
    case u of
      MCE (p:q:_) -> (p,q)
      _ -> (u,u)

-- | Multiple channel expansion for two inputs.
mce3 :: UGen -> UGen -> UGen -> UGen
mce3 x y z = mce [x,y,z]

-- | Number of channels to expand to.
mceDegree :: UGen -> Int
mceDegree u =
    case u of
      MCE l -> length l
      MRG x _ -> mceDegree x
      _ -> error "mceDegree: illegal ugen"

-- | Extend UGen to specified degree.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n u =
    case u of
      MCE l -> take n (cycle l)
      MRG x y -> let (r:rs) = mceExtend n x
                 in MRG r y : rs
      _ -> replicate n u

-- | Apply MCE transform to a list of inputs.
mceInputTransform :: [UGen] -> Maybe [[UGen]]
mceInputTransform i =
    if any isMCE i
    then let n = maximum (map mceDegree (filter isMCE i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

-- | Build a UGen after MCE transformation of inputs.
mceBuild :: ([UGen] -> UGen) -> [UGen] -> UGen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> MCE (map (mceBuild f) i')

-- | Apply a function to each channel at a unit generator.
mceMap :: (UGen -> UGen) -> UGen -> UGen
mceMap f u = mce (map f (mceChannels u))

-- | Apply UGen list operation on MCE contents.
mceEdit :: ([UGen] -> [UGen]) -> UGen -> UGen
mceEdit f u =
    case u of
      MCE l -> MCE (f l)
      _ -> error "mceEdit: non MCE value"

-- | Reverse order of channels at MCE.
mceReverse :: UGen -> UGen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n u =
    case u of
      MCE l -> l !! n
      _ -> error "mceChannel: non MCE value"

-- | Output channels of UGen as a list.
mceChannels :: UGen -> [UGen]
mceChannels u =
    case u of
      MCE l -> l
      MRG x y -> let (r:rs) = mceChannels x in MRG r y : rs
      _ -> [u]

-- | Transpose rows and columns, ie. {{a,b},{c,d}} to {{a,c},{b,d}}.
mceTranspose :: UGen -> UGen
mceTranspose = mce . map mce . transpose . map mceChannels . mceChannels

-- | Collapse mce by summing (see also mix and mixN).
mceSum :: UGen -> UGen
mceSum = sum . mceChannels

-- * Multiple root graphs

-- | Multiple root graph constructor.
mrg :: [UGen] -> UGen
mrg u =
    case u of
      [] -> error "mrg: null"
      [x] -> x
      (x:xs) -> MRG x (mrg xs)

-- * Labels

-- | Lift a 'String' to a UGen label (ie. for 'poll').
label :: String -> UGen
label = Label

-- | Are lists of equal length?
--
-- > equal_length_p ["t1","t2"] == True
-- > equal_length_p ["t","t1","t2"] == False
equal_length_p :: [[a]] -> Bool
equal_length_p = (== 1) . length . nub . map length

-- | Unpack a label to a length prefixed list of 'Constant's.  There
-- is a special case for mce nodes, but it requires labels to be equal
-- length.  Properly, 'poll' would not unpack the label, it would be
-- done by the synthdef builder.
unpackLabel :: UGen -> [UGen]
unpackLabel u =
    case u of
      Label s -> let q = fromEnum '?'
                     f c = if C.isAscii c then fromEnum c else q
                     s' = map (fromIntegral . f) s
                     n = fromIntegral (length s)
                 in n : s'
      MCE x -> let x' = map unpackLabel x
               in if equal_length_p x'
                  then map mce (transpose x')
                  else error (show ("unpackLabel: mce length /=",x))
      _ -> error (show ("unpackLabel: non-label",u))

-- * Unit generator function builders

-- | Apply proxy transformation if required.
proxify :: UGen -> UGen
proxify u =
    case ugenType u of
    MCE_U -> mce (map proxify (mceProxies u))
    MRG_U -> mrg [proxify (mrgLeft u), mrgRight u]
    Primitive_U ->
        let o = ugenOutputs u
        in case o of
             (_:_:_) -> mce (map (proxy u) [0..(length o - 1)])
             _ -> u
    Constant_U -> u
    _ -> error "proxify: illegal ugen"

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf u =
    case ugenType u of
      Constant_U -> IR
      Control_U -> controlOperatingRate u
      Label_U -> IR
      Primitive_U -> ugenRate u
      Proxy_U -> rateOf (proxySource u)
      MCE_U -> maximum (map rateOf (mceChannels u))
      MRG_U -> rateOf (mrgLeft u)

-- | True if input is a sink 'UGen', ie. has no outputs.
is_sink :: UGen -> Bool
is_sink u =
    case ugenType u of
      Primitive_U -> null (ugenOutputs u)
      MCE_U -> all is_sink (mceProxies u)
      MRG_U -> is_sink (mrgLeft u)
      _ -> False

-- | Ensure input 'UGen' is valid, ie. not a sink.
check_input :: UGen -> UGen
check_input u =
    if is_sink u
    then error ("illegal input: " ++ show u)
    else u

-- | Construct proxied and multiple channel expanded UGen.
mkUGen :: Maybe ([Double] -> Double) -> [Rate] -> Maybe Rate ->
          String -> [UGen] -> Int -> Special -> UGenId -> UGen
mkUGen cf rs r nm i o s z =
    let f h = let r' = fromMaybe (maximum (map rateOf h)) r
                  o' = replicate o r'
                  u = Primitive r' nm h o' s z
              in if r' `elem` rs
                 then case cf of
                        Just cf' ->
                            if all isConstant h
                            then Constant (cf' (map constantValue h))
                            else u
                        Nothing -> u
                 else error ("mkUGen: rate restricted: " ++ show (r,rs,nm))
    in proxify (mceBuild f (map check_input i))

-- | Set of all 'Rate' values.
all_rates :: [Rate]
all_rates = [minBound .. maxBound]

-- | 'UGenId' used for deterministic UGens.
no_id :: UGenId
no_id = NoId

-- | Operator UGen constructor.
mkOperator :: ([Double] -> Double) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    mkUGen (Just f) all_rates Nothing c i 1 (Special s) no_id

-- | Unary math constructor with constant optimization.
mkUnaryOperator :: Unary -> (Double -> Double) -> UGen -> UGen
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
mkBinaryOperator_optimize :: Binary -> (Double -> Double -> Double) ->
                             (Either Double Double -> Bool) ->
                             UGen -> UGen -> UGen
mkBinaryOperator_optimize i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (Constant a',_) -> if o (Left a') then Just b else Nothing
             (_,Constant b') -> if o (Right b') then Just a else Nothing
             _ -> Nothing
   in maybe (mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)) id r

-- | Binary math constructor with constant optimization.
mkBinaryOperator :: Binary -> (Double -> Double -> Double) ->
                    UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

-- | Oscillator constructor with constrained set of operating 'Rate's.
mk_osc :: [Rate] -> UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mk_osc rs z r c i o =
    if r `elem` rs
    then mkUGen Nothing rs (Just r) c i o (Special 0) z
    else error ("mk_osc: rate restricted: " ++ show (r, rs, c))

-- | Oscillator constructor with 'all_rates'.
mkOsc :: Rate -> String -> [UGen] -> Int -> UGen
mkOsc = mk_osc all_rates no_id

-- | Oscillator constructor, rate restricted variant.
mkOscR :: [Rate] -> Rate -> String -> [UGen] -> Int -> UGen
mkOscR rs = mk_osc rs no_id

toUId :: (ID a) => a -> UGenId
toUId = UId . resolveID

-- | Rate restricted oscillator constructor, setting identifier.
mkOscIdR :: (ID a) => [Rate] -> a -> Rate -> String -> [UGen] -> Int -> UGen
mkOscIdR rr z r nm = mk_osc rr (toUId z) r nm

-- | Oscillator constructor, setting identifier.
mkOscId :: (ID a) => a -> Rate -> String -> [UGen] -> Int -> UGen
mkOscId z r nm = mk_osc all_rates (toUId z) r nm

-- | Provided 'UGenId' variant of 'mkOscMCE'.
mk_osc_mce :: UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mk_osc_mce z r c i j =
    let i' = i ++ mceChannels j
    in mk_osc all_rates z r c i'

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mk_osc_mce no_id

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: ID a => a -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z r nm = mk_osc_mce (toUId z) r nm

-- | Rate constrained filter 'UGen' constructor.
mk_filter :: [Rate] -> UGenId -> String -> [UGen] -> Int -> UGen
mk_filter rs z c i o = mkUGen Nothing rs Nothing c i o (Special 0) z

-- | Filter 'UGen' constructor.
mkFilter :: String -> [UGen] -> Int -> UGen
mkFilter = mk_filter all_rates no_id

-- | Filter UGen constructor.
mkFilterR :: [Rate] -> String -> [UGen] -> Int -> UGen
mkFilterR rs = mk_filter rs no_id

-- | Filter UGen constructor.
mkFilterId :: (ID a) => a -> String -> [UGen] -> Int -> UGen
mkFilterId z nm = mk_filter all_rates (toUId z) nm

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: String -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o =
    let r = rateOf (i !! k)
    in mkUGen Nothing all_rates (Just r) c i o (Special 0) no_id

-- | Provided 'UGenId' filter with 'mce' input.
mk_filter_mce :: [Rate] -> UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mk_filter_mce rs z c i j = mk_filter rs z c (i ++ mceChannels j)

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCER :: [Rate] -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCER rs = mk_filter_mce rs no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE = mk_filter_mce all_rates no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: ID a => a -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCEId z nm = mk_filter_mce all_rates (toUId z) nm

-- | Information unit generators are very specialized.
mkInfo :: String -> UGen
mkInfo name = mkOsc IR name [] 1

-- Unit generators are numbers.
instance Num UGen where
    negate = mkUnaryOperator Neg negate
    (+) = mkBinaryOperator_optimize Add (+) (`elem` [Left 0,Right 0])
    (-) = mkBinaryOperator_optimize Sub (-) ((==) (Right 0))
    (*) = mkBinaryOperator_optimize Mul (*) (`elem` [Left 1,Right 1])
    abs = mkUnaryOperator Abs abs
    signum = mkUnaryOperator Sign signum
    fromInteger = Constant . fromInteger

-- Unit generators are fractional.
instance Fractional UGen where
    recip = mkUnaryOperator Recip recip
    (/) = mkBinaryOperator_optimize FDiv (/) ((==) (Right 1))
    fromRational = Constant . fromRational

-- Unit generators are floating point.
instance Floating UGen where
    pi = Constant pi
    exp = mkUnaryOperator Exp exp
    log = mkUnaryOperator Log log
    sqrt = mkUnaryOperator Sqrt sqrt
    (**) = mkBinaryOperator_optimize Pow (**) ((==) (Right 1))
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

-- Unit generators are real.
instance Real UGen where
    toRational (Constant n) = toRational n
    toRational _ = error "toRational at non-constant UGen"

-- Unit generators are integral.
instance Integral UGen where
    quot = mkBinaryOperator IDiv (error "ugen: quot")
    rem = mkBinaryOperator Mod (error "ugen: rem")
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator IDiv (error "ugen: div")
    mod = mkBinaryOperator Mod (error "ugen: mod")
    toInteger (Constant n) = floor n
    toInteger _ = error "toInteger at non-constant UGen"

-- Unit generators are orderable.
instance Ord UGen where
    (Constant a) < (Constant b) = a < b
    _ < _ = error "< at UGen is partial, see <*"
    (Constant a) <= (Constant b) = a <= b
    _ <= _ = error "<= at UGen is partial, see <=*"
    (Constant a) > (Constant b) = a < b
    _ > _ = error "> at UGen is partial, see >*"
    (Constant a) >= (Constant b) = a >= b
    _ >= _ = error ">= at UGen is partial, see >=*"
    min = mkBinaryOperator Min min
    max = mkBinaryOperator Max max

-- Unit generators are enumerable.
instance Enum UGen where
    succ u = u + 1
    pred u = u - 1
    toEnum = constant
    fromEnum (Constant n) = truncate n
    fromEnum _ = error "cannot enumerate non-constant UGens"
    enumFrom = iterate (+1)
    enumFromThen n m = iterate (+(m-n)) n
    enumFromTo n m = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m =
        let p = if n' >= n then (>=) else (<=)
        in takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')

-- Unit generators are stochastic.
instance Random UGen where
    randomR (Constant l, Constant r) g =
        let (n, g') = randomR (l,r) g
        in (Constant n, g')
    randomR _ _ = error "randomR: non constant (l,r)"
    random = randomR (-1.0, 1.0)

-- * Bitwise

bitAnd :: UGen -> UGen -> UGen
bitAnd = mkBinaryOperator BitAnd undefined

bitOr :: UGen -> UGen -> UGen
bitOr = mkBinaryOperator BitOr undefined

bitXOr :: UGen -> UGen -> UGen
bitXOr = mkBinaryOperator BitXor undefined

bitNot :: UGen -> UGen
bitNot = mkUnaryOperator BitNot undefined

shiftLeft :: UGen -> UGen -> UGen
shiftLeft = mkBinaryOperator ShiftLeft undefined

shiftRight :: UGen -> UGen -> UGen
shiftRight = mkBinaryOperator ShiftRight undefined

unsignedShift :: UGen -> UGen -> UGen
unsignedShift = mkBinaryOperator UnsignedShift undefined

instance Bits UGen where
    (.&.) = mkBinaryOperator BitAnd undefined
    (.|.) = mkBinaryOperator BitOr undefined
    xor = mkBinaryOperator BitXor undefined
    complement = mkUnaryOperator BitNot undefined
    shift = error "Bits/UGen is partial"
    rotate = error "Bits/UGen is partial"
    bitSize = error "Bits/UGen is partial"
    isSigned _ = True

(.<<.) :: UGen -> UGen -> UGen
(.<<.) = shiftLeft

(.>>.) :: UGen -> UGen -> UGen
(.>>.) = shiftRight
