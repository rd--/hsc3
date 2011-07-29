module Sound.SC3.UGen.UGen where

import Control.Monad
import qualified Data.HashTable as H
import Data.List
import Sound.SC3.Identifier
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UId
import System.Random

-- | Unit generator.
data UGen = Constant { constantValue :: Double }
          | Control { controlOperatingRate :: Rate
                    , controlName :: String
                    , controlDefault :: Double
                    , controlTriggered :: Bool }
          | Primitive { ugenRate :: Rate
                      , ugenName :: String
                      , ugenInputs :: [UGen]
                      , ugenOutputs :: [Output]
                      , ugenSpecial :: Special
                      , ugenId :: Int }
          | Proxy { proxySource :: UGen
                  , proxyIndex :: Int }
          | MCE { mceProxies :: [UGen] }
          | MRG { mrgLeft :: UGen
                , mrgRight :: UGen }
            deriving (Eq, Show)

-- | Hash function for unit generators.
hashUGen :: UGen -> Int
hashUGen = fromIntegral . H.hashString . show

instance ID UGen where
    resolveID = hashUGen

-- | Unit generator output descriptor.
type Output = Rate

-- | Operating mode of unary and binary operators.
newtype Special = Special Int
    deriving (Eq, Show)

-- * Unit generator node constructors

defaultID :: Int
defaultID = (-1)

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
mce xs = if null xs then error "mce: empty list" else MCE xs

-- | Multiple root graph node constructor.
mrg2 :: UGen -> UGen -> UGen
mrg2 = MRG

-- | Unit generator proxy node constructor.
proxy :: UGen -> Int -> UGen
proxy = Proxy

-- * Unit generator node predicates

-- | Constant node predicate.
isConstant :: UGen -> Bool
isConstant u =
    case u of
      Constant _ -> True
      _ -> False

-- | Control node predicate.
isControl :: UGen -> Bool
isControl u =
    case u of
      Control _ _ _ _ -> True
      _ -> False

-- | Unit generator primitive node predicate.
isPrimitive :: UGen -> Bool
isPrimitive u =
    case u of
      Primitive _ _ _ _ _ _ -> True
      _ -> False

-- | Proxy node predicate.
isProxy :: UGen -> Bool
isProxy u =
    case u of
      Proxy _ _ -> True
      _ -> False

-- | Multiple channel expansion node predicate.
isMCE :: UGen -> Bool
isMCE u =
    case u of
      MCE _ -> True
      _ -> False

-- | MRG predicate.
isMRG :: UGen -> Bool
isMRG u =
    case u of
      MRG _ _ -> True
      _ -> False

-- * Multiple channel expansion

-- | Multiple channel expansion for two inputs.
mce2 :: UGen -> UGen -> UGen
mce2 x y = mce [x, y]

-- | Clone a unit generator (mce . replicateM).
clone :: (UId m) => Int -> m UGen -> m UGen
clone n = liftM mce . replicateM n

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
      Just i' -> MCE (map f i')

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
      [] -> undefined
      [x] -> x
      (x:xs) -> MRG x (mrg xs)

-- * Unit generator function builders

-- | Apply proxy transformation if required.
proxify :: UGen -> UGen
proxify u
    | isMCE u = mce (map proxify (mceProxies u))
    | isMRG u = mrg [proxify (mrgLeft u), mrgRight u]
    | isPrimitive u =
        let o = ugenOutputs u
        in case o of
             (_:_:_) -> mce (map (proxy u) [0..(length o - 1)])
             _ -> u
    | isConstant u = u
    | otherwise = error "proxify: illegal ugen"

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf u
    | isConstant u = IR
    | isControl u = controlOperatingRate u
    | isPrimitive u = ugenRate u
    | isProxy u = rateOf (proxySource u)
    | isMCE u = error "rateOf: mce"
    | isMRG u = rateOf (mrgLeft u)
    | otherwise = undefined

-- True is input is a sink UGen, ie. has no outputs.
is_sink :: UGen -> Bool
is_sink u
    | isPrimitive u = null (ugenOutputs u)
    | isMCE u = all is_sink (mceProxies u)
    | isMRG u = is_sink (mrgLeft u)
    | otherwise = False

-- Ensure input UGen is valid, ie. not a sink.
check_input :: UGen -> UGen
check_input u =
    if is_sink u
    then error ("illegal input: " ++ show u)
    else u

-- | Construct proxied and multiple channel expanded UGen.
mkUGen :: (ID a) => Maybe ([Double] -> Double) -> [Rate] ->
          Maybe Rate -> String -> [UGen] -> Int -> Special -> a -> UGen
mkUGen cf rs r nm i o s z =
    let f h = let r'' = case r of
                          Nothing -> maximum (map rateOf h)
                          Just r' -> r'
                  o' = replicate o r''
                  u = Primitive r'' nm h o' s (resolveID z)
              in if r'' `elem` rs
                 then case cf of
                        Just cf' ->
                            if all isConstant h
                            then Constant (cf' (map constantValue h))
                            else u
                        Nothing -> u
                 else error ("mkUGen: rate restricted: " ++ show (r,rs,nm))
    in proxify (mceBuild f (map check_input i))

all_rates :: [Rate]
all_rates = [minBound .. maxBound]

-- | Operator UGen constructor.
mkOperator :: ([Double] -> Double) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    mkUGen (Just f) all_rates Nothing c i 1 (Special s) defaultID

-- | Unary math constructor with constant optimization.
mkUnaryOperator :: Unary -> (Double -> Double) -> UGen -> UGen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimization.
mkBinaryOperator :: Binary -> (Double -> Double -> Double) ->
                    UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

mk_osc :: (ID a) =>
          [Rate] -> a ->
          Rate -> String -> [UGen] -> Int -> UGen
mk_osc rs z r c i o =
    if r `elem` rs
    then mkUGen Nothing rs (Just r) c i o (Special 0) z
    else error ("mk_osc: rate restricted: " ++ show (r, rs, c))

-- | Oscillator constructor.
mkOsc :: Rate -> String -> [UGen] -> Int -> UGen
mkOsc = mk_osc [minBound .. maxBound] defaultID

-- | Oscillator constructor, rate restricted variant.
mkOscR :: [Rate] -> Rate -> String -> [UGen] -> Int -> UGen
mkOscR rs = mk_osc rs defaultID

-- | Oscillator constructor, setting identifier.
mkOscId :: (ID a) =>
           a -> Rate -> String -> [UGen] -> Int -> UGen
mkOscId = mk_osc [minBound .. maxBound]

mk_osc_mce :: (ID a) =>
              a -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mk_osc_mce z r c i j =
    let i' = i ++ mceChannels j
    in mk_osc [minBound .. maxBound] z r c i'

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mk_osc_mce defaultID

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: ID a => a -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId = mk_osc_mce

mk_filter :: ID a => [Rate] -> a -> String -> [UGen] -> Int -> UGen
mk_filter rs z c i o = mkUGen Nothing rs Nothing c i o (Special 0) z

-- | Filter UGen constructor.
mkFilter :: String -> [UGen] -> Int -> UGen
mkFilter = mk_filter all_rates defaultID

-- | Filter UGen constructor.
mkFilterR :: [Rate] -> String -> [UGen] -> Int -> UGen
mkFilterR rs = mk_filter rs defaultID

-- | Filter UGen constructor.
mkFilterId :: (ID a) => a -> String -> [UGen] -> Int -> UGen
mkFilterId = mk_filter all_rates

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: String -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o =
    let r = rateOf (i !! k)
    in mkUGen Nothing all_rates (Just r) c i o (Special 0) defaultID

mk_filter_mce :: (ID a) => [Rate] -> a ->
                 String -> [UGen] -> UGen -> Int -> UGen
mk_filter_mce rs z c i j = mk_filter rs z c (i ++ mceChannels j)

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCER :: [Rate] -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCER rs = mk_filter_mce rs defaultID

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE = mk_filter_mce all_rates defaultID

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: ID a => a -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCEId = mk_filter_mce all_rates

-- | Information unit generators are very specialized.
mkInfo :: String -> UGen
mkInfo name = mkOsc IR name [] 1

-- Unit generators are numbers.
instance Num UGen where
    negate = mkUnaryOperator Neg negate
    (+) = mkBinaryOperator Add (+)
    (-) = mkBinaryOperator Sub (-)
    (*) = mkBinaryOperator Mul (*)
    abs = mkUnaryOperator Abs abs
    signum = mkUnaryOperator Sign signum
    fromInteger = Constant . fromInteger

-- Unit generators are fractional.
instance Fractional UGen where
    recip = mkUnaryOperator Recip recip
    (/) = mkBinaryOperator FDiv (/)
    fromRational = Constant . fromRational

-- Unit generators are floating point.
instance Floating UGen where
    pi = Constant pi
    exp = mkUnaryOperator Exp exp
    log = mkUnaryOperator Log log
    sqrt = mkUnaryOperator Sqrt sqrt
    (**) = mkBinaryOperator Pow (**)
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
    quot = mkBinaryOperator IDiv undefined
    rem = mkBinaryOperator Mod undefined
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator IDiv undefined
    mod = mkBinaryOperator Mod undefined
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
