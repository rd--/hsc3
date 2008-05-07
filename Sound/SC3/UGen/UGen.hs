module Sound.SC3.UGen.UGen where

import Control.Monad
import Data.List
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UId
import System.Random

-- | Unit generator.
data UGen = Constant { constantValue :: Double }
          | Control { controlOperatingRate :: Rate
                    , controlName :: String
                    , controlDefault :: Double }
          | Primitive { ugenRate :: Rate
                      , ugenName :: String
                      , ugenInputs :: [UGen]
                      , ugenOutputs :: [Output]
                      , ugenSpecial :: Special
                      , ugenId :: Maybe UGenId }
          | Proxy { proxySource :: UGen
                  , proxyIndex :: Int }
          | MCE { mceProxies :: [UGen] }
          | MRG { mrgLeft :: UGen 
                , mrgRight :: UGen }
            deriving (Eq, Show)

-- | Unit generator output descriptor.
type Output = Rate

-- | Operating mode of unary and binary operators.
newtype Special = Special Int 
    deriving (Eq, Show)

-- | Identifier for non-functional unit generators.
newtype UGenId = UGenId Int 
    deriving (Eq, Show)

-- * Unit generator node constructors.

-- | Unit generator identifier constructor.
uid :: Int -> UGenId
uid = UGenId

-- | Constant value node constructor.
constant :: (Real a) => a -> UGen
constant = Constant . realToFrac

-- | Control input node constructor.
control :: Rate -> String -> Double -> UGen
control = Control

-- | Multiple channel expansion node constructor.
mce :: [UGen] -> UGen
mce [] = error "mce: empty list"
mce xs = MCE xs

-- | Multiple root graph node constructor.
mrg2 :: UGen -> UGen -> UGen
mrg2 = MRG

-- | Unit generator proxy node constructor.
proxy :: UGen -> Int -> UGen
proxy = Proxy

-- * Unit generator node predicates.

-- | Constant node predicate.
isConstant :: UGen -> Bool
isConstant (Constant _) = True
isConstant _            = False

-- | Control node predicate.
isControl :: UGen -> Bool
isControl (Control _ _ _) = True
isControl _               = False

-- | Unit generator primitive node predicate.
isUGen :: UGen -> Bool
isUGen (Primitive _ _ _ _ _ _) = True
isUGen _ = False

-- | Proxy node predicate.
isProxy :: UGen -> Bool
isProxy (Proxy _ _) = True
isProxy _           = False

-- | Multiple channel expansion node predicate.
isMCE :: UGen -> Bool
isMCE (MCE _) = True
isMCE _       = False

-- | MRG predicate.
isMRG :: UGen -> Bool
isMRG (MRG _ _) = True
isMRG _ = False

-- * Multiple channel expansion.

-- | Multiple channel expansion for two inputs.
mce2 :: UGen -> UGen -> UGen
mce2 x y = mce [x, y]

-- | Clone a unit generator (mce . replicateM).
clone :: (UId m) => Int -> m UGen -> m UGen
clone n u = liftM mce (replicateM n u)

-- | Number of channels to expand to.
mceDegree :: UGen -> Int
mceDegree (MCE l) = length l
mceDegree (MRG u _) = mceDegree u
mceDegree _ = error "mceDegree: illegal ugen"

-- | Extend UGen to specified degree.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n (cycle l)
mceExtend n (MRG x y) = (MRG r y : rs) where (r:rs) = mceExtend n x
mceExtend n u = replicate n u

-- | Apply MCE transformation.
mceTransform :: UGen -> UGen
mceTransform (Primitive r n i o s d) = MCE (map f i')
    where f j = Primitive r n j o s d
          upr = maximum (map mceDegree (filter isMCE i))
          i' = transpose (map (mceExtend upr) i)
mceTransform _ = error "mceTransform: illegal ugen"

-- | Apply MCE transformation if required.
mceExpand :: UGen -> UGen
mceExpand (MCE l) = MCE (map mceExpand l)
mceExpand (MRG x y) = MRG (mceExpand x) y
mceExpand u = if required u then mceExpand (mceTransform u) else u
    where required (Primitive _ _ i _ _ _) = not (null (filter isMCE i))
          required _ = False

-- | Apply UGen list operation on MCE contents.
mceEdit :: ([UGen] -> [UGen]) -> UGen -> UGen
mceEdit f (MCE l) = MCE (f l)
mceEdit _ _ = error "mceEdit: non MCE value"

-- | Reverse order of channels at MCE.
mceReverse :: UGen -> UGen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n (MCE l) = l !! n
mceChannel _ _ = error "mceChannel: non MCE value"

-- | Output channels of UGen as a list.
mceChannels :: UGen -> [UGen]
mceChannels (MCE l) = l
mceChannels (MRG x y) = (MRG r y) : rs where (r:rs) = mceChannels x
mceChannels u = [u]

-- | Transpose rows and columns, ie. {{a,b},{c,d}} to {{a,c},{b,d}}.
mceTranspose :: UGen -> UGen
mceTranspose u = mce (map mce (transpose (map mceChannels (mceChannels u))))

-- * Multiple root graphs.

-- | Multiple root graph constructor.
mrg :: [UGen] -> UGen
mrg [] = undefined
mrg [x] = x
mrg (x:xs) = MRG x (mrg xs)

-- * Unit generator function builders.

-- | Apply proxy transformation if required.
proxify :: UGen -> UGen
proxify u 
    | isMCE u = mce (map proxify (mceProxies u))
    | isMRG u = mrg [proxify (mrgLeft u), mrgRight u]
    | isUGen u = let o = ugenOutputs u
                 in case o of
                      (_:_:_) -> mce (map (proxy u) [0..(length o - 1)])
                      _ -> u
    | otherwise = error "proxify: illegal ugen"

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf u
    | isConstant u = IR
    | isControl u = controlOperatingRate u
    | isUGen u = ugenRate u
    | isProxy u = rateOf (proxySource u)
    | isMCE u = maximum (map rateOf (mceProxies u))
    | isMRG u = rateOf (mrgLeft u)
    | otherwise = undefined

-- True is input is a sink UGen, ie. has no outputs.
is_sink :: UGen -> Bool
is_sink u
    | isUGen u = null (ugenOutputs u)
    | isMCE u = all is_sink (mceProxies u)
    | isMRG u = is_sink (mrgLeft u)
    | otherwise = False

-- Ensure input UGen is valid, ie. not a sink.
check_input :: UGen -> UGen
check_input u = if is_sink u 
                then error ("illegal input" ++ show u) 
                else u

-- | Construct proxied and multiple channel expanded UGen.
mkUGen :: Rate -> String -> [UGen] -> [Output] -> Special -> Maybe UGenId -> UGen
mkUGen r n i o s z = proxify (mceExpand u)
    where u = Primitive r n (map check_input i) o s z

-- | Operator UGen constructor.
mkOperator :: String -> [UGen] -> Int -> UGen
mkOperator c i s = mkUGen r c i [r] (Special s) Nothing
    where r = maximum (map rateOf i)

-- | Unary math constructor with constant optimization.
mkUnaryOperator :: Unary -> (Double -> Double) -> UGen -> UGen
mkUnaryOperator i f a 
    | isConstant a = constant (f (constantValue a))
    | otherwise = mkOperator "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimization.
mkBinaryOperator :: Binary -> (Double -> Double -> Double) -> UGen -> UGen -> UGen
mkBinaryOperator i f a b 
    | isConstant a && isConstant b = let a' = constantValue a
                                         b' = constantValue b
                                     in constant (f a' b')
    | otherwise = mkOperator "BinaryOpUGen" [a, b] (fromEnum i)

mk_osc :: Maybe UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mk_osc z r c i o = mkUGen r c i (replicate o r) (Special 0) z

-- | Oscillator constructor.
mkOsc :: Rate -> String -> [UGen] -> Int -> UGen
mkOsc = mk_osc Nothing

-- | Oscillator constructor, setting identifier.
mkOscId :: UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mkOscId z = mk_osc (Just z)

mk_osc_mce :: Maybe UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mk_osc_mce z r c i j o = mk_osc z r c (i ++ mceChannels j) o

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mk_osc_mce Nothing

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z = mk_osc_mce (Just z)

mk_filter :: Maybe UGenId -> String -> [UGen] -> Int -> UGen
mk_filter z c i o = mkUGen r c i o' (Special 0) z
    where r = maximum (map rateOf i)
          o'= replicate o r

-- | Filter UGen constructor.
mkFilter :: String -> [UGen] -> Int -> UGen
mkFilter = mk_filter Nothing

-- | Filter UGen constructor.
mkFilterId :: UGenId -> String -> [UGen] -> Int -> UGen
mkFilterId z = mk_filter (Just z)

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: String -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o = mkUGen r c i o' (Special 0) Nothing
    where r = rateOf (i !! k)
          o' = replicate o r

mk_filter_mce :: Maybe UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mk_filter_mce z c i j o = mk_filter z c (i ++ mceChannels j) o

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE = mk_filter_mce Nothing

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCEId z = mk_filter_mce (Just z)

-- | Information unit generators are very specialized.
mkInfo :: String -> UGen
mkInfo name = mkOsc IR name [] 1

-- Unit generators are numbers.
instance Num UGen where
    negate         = mkUnaryOperator Neg negate
    (+)            = mkBinaryOperator Add (+)
    (-)            = mkBinaryOperator Sub (-)
    (*)            = mkBinaryOperator Mul (*)
    abs            = mkUnaryOperator Abs abs
    signum         = mkUnaryOperator Sign signum
    fromInteger    = Constant . fromInteger

-- Unit generators are fractional.
instance Fractional UGen where
    recip          = mkUnaryOperator Recip recip
    (/)            = mkBinaryOperator FDiv (/)
    fromRational   = Constant . fromRational

-- Unit generators are floating point.
instance Floating UGen where
    pi             = Constant pi
    exp            = mkUnaryOperator Exp exp
    log            = mkUnaryOperator Log log
    sqrt           = mkUnaryOperator Sqrt sqrt
    (**)           = mkBinaryOperator Pow (**)
    logBase a b    = log b / log a
    sin            = mkUnaryOperator Sin sin
    cos            = mkUnaryOperator Cos cos
    tan            = mkUnaryOperator Tan tan
    asin           = mkUnaryOperator ArcSin asin
    acos           = mkUnaryOperator ArcCos acos
    atan           = mkUnaryOperator ArcTan atan
    sinh           = mkUnaryOperator SinH sinh
    cosh           = mkUnaryOperator CosH cosh
    tanh           = mkUnaryOperator TanH tanh
    asinh x        = log (sqrt (x*x+1) + x)
    acosh x        = log (sqrt (x*x-1) + x)
    atanh x        = (log (1+x) - log (1-x)) / 2

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
    (Constant a) <  (Constant b) = a <  b
    _            <  _            = error "< at UGen is partial, see <*"
    (Constant a) <= (Constant b) = a <= b
    _            <= _            = error "<= at UGen is partial, see <=*"
    (Constant a) >  (Constant b) = a <  b
    _            >  _            = error "> at UGen is partial, see >*"
    (Constant a) >= (Constant b) = a >= b
    _            >= _            = error ">= at UGen is partial, see >=*"
    min = mkBinaryOperator Min min
    max = mkBinaryOperator Max max

-- Unit generators are enumerable.
instance Enum UGen where
    succ u                = u + 1
    pred u                = u - 1
    toEnum i              = constant i
    fromEnum (Constant n) = truncate n
    fromEnum _            = error "cannot enumerate non-constant UGens"
    enumFrom              = iterate (+1)
    enumFromThen n m      = iterate (+(m-n)) n
    enumFromTo n m        = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m = takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')
        where p = if n' >= n then (>=) else (<=)

-- Unit generators are stochastic.
instance Random UGen where
    randomR (Constant l, Constant r) g = let (n, g') = randomR (l,r) g
                                         in (Constant n, g')
    randomR _ _ = error "randomR: non constant (l,r)"
    random g = randomR (-1.0, 1.0) g
