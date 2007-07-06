module Sound.SC3.UGen.UGen ( Name, UGenId(..), UGen(..), Output, Special(..)
                           , mkOsc, mkOscMCE
                           , mkOscId, mkOscMCEId
                           , mkFilter, mkFilterMCE, mkFilterKeyed
                           , mkFilterId
                           , isConstant, isControl, isUGen, isProxy, isMRG, isMCE
                           , mceChannel, mceReverse, mceDegree
                           , nodes, hasOutputs
                           , liftU, liftU2, liftU3, liftU4
                           , mix, mixFill
                           , clone ) where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.Operator (Unary(..),Binary(..))
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.UId

import Prelude hiding (EQ, GT, LT)
import Data.List (transpose)
import Control.Monad (liftM, replicateM)
import System.Random (Random, randomR, random)

type Name    = String
type Output  = Rate
newtype Special = Special Int deriving (Eq, Show)
newtype UGenId = UGenId Int deriving (Eq, Show)
data UGen    = Constant { constantValue :: Double }
             | Control { controlRate_ :: Rate
                       , controlName :: Name
                       , controlDefault :: Double }
             | UGen { ugenRate :: Rate
                    , ugenName :: Name
                    , ugenInputs :: [UGen]
                    , ugenOuputs :: [Output]
                    , ugenSpecial :: Special
                    , ugenId :: UGenId }
             | Proxy { proxySource :: UGen
                     , proxyIndex :: Int }
             | MCE { mceProxies :: [UGen] }
             | MRG { mrgRoots :: [UGen] }
               deriving (Eq, Show)

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf (Constant _)        =  IR
rateOf (Control r _ _)     =  r
rateOf (UGen r _ _ _ _ _)  =  r
rateOf (Proxy u _)         =  rateOf u
rateOf (MCE u)             =  maximum (map rateOf u)
rateOf (MRG _)             =  error "rateOf: applied to MRG"

-- | The list of all UGens referenced in a UGen graph.
nodes :: UGen -> [UGen]
nodes u@(UGen _ _ i _ _ _) =  u : concatMap nodes i
nodes (Proxy u _)          =  u : nodes u
nodes (MCE u)              =  concatMap nodes u
nodes (MRG u)              =  concatMap nodes u
nodes u                    =  [u]

-- | Constant predicate.
isConstant :: UGen -> Bool
isConstant (Constant _) = True
isConstant _            = False

-- | Control predicate.
isControl :: UGen -> Bool
isControl (Control _ _ _) = True
isControl _               = False

-- | UGen predicate.
isUGen :: UGen -> Bool
isUGen (UGen _ _ _ _ _ _) = True
isUGen _                  = False

-- | Proxy predicate.
isProxy :: UGen -> Bool
isProxy (Proxy _ _) = True
isProxy _           = False

-- | MCE predicate.
isMCE :: UGen -> Bool
isMCE (MCE _) = True
isMCE _       = False

-- | MRG predicate.
isMRG :: UGen -> Bool
isMRG (MRG _) = True
isMRG _       = False

-- | True if the 'UGen' has output ports (ie. is not a sink UGen).
hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = not (null o)
hasOutputs (MCE l)            = any hasOutputs l
hasOutputs (MRG l)            = any hasOutputs l
hasOutputs (Proxy _ _)        = True
hasOutputs _                  = False

-- | Apply proxy transformation if required.
proxy :: UGen -> UGen
proxy (MCE l) = MCE (map proxy l)
proxy u@(UGen _ _ _ o _ _) =
   case o of
      (_:_:_) -> MCE (map (Proxy u) [0..(length o - 1)])
      _       -> u
proxy _       = error "proxy: illegal ugen"

-- * Multiple Channel Expansion

-- | Number of channels to expand to.
mceDegree :: UGen -> Int
mceDegree (UGen _ _ _ _ _ _) = 1
mceDegree (MCE l) = length l
mceDegree _       = error "mceDegree: illegal ugen"

-- | Is expansion required, ie. are any inputs MCE values.
mceRequired :: UGen -> Bool
mceRequired (UGen _ _ i _ _ _) = not (null (filter isMCE i))
mceRequired (MCE l)            = any mceRequired l
mceRequired _                  = False

-- | Extend UGen to specified degree.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n (cycle l)
mceExtend n u       = replicate n u

-- | Apply MCE transformation.
mceTransform :: UGen -> UGen
mceTransform (UGen r n i o s d) = MCE (map f i')
    where f j = UGen r n j o s d
          upr = maximum (map mceDegree (filter isMCE i))
          i'  = transpose (map (mceExtend upr) i)
mceTransform _ = error "mceTransform: illegal ugen"

-- | Apply MCE transformation if required.
mceExpand :: UGen -> UGen
mceExpand (MCE l) = MCE (map mceExpand l)
mceExpand u       = if mceRequired u then mceExpand (mceTransform u) else u

-- | Reverse order of channels at MCE.
mceReverse :: UGen -> UGen
mceReverse (MCE l) = MCE (reverse l)
mceReverse _       = error "mceReverse: non MCE value"

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n (MCE l) = l !! n
mceChannel _ _       = error "mceChannel: non MCE value"

-- | Output channels of UGen as a list.
mceChannels :: UGen -> [UGen]
mceChannels (MCE l) = l
mceChannels u       = [u]

-- * UGen Constructors.

liftU :: (UId m) => (UGenId -> a -> UGen) -> (a -> m UGen)
liftU f a = do n <- generateUId
               return (f (UGenId n) a)

liftU2 :: (UId m) => (UGenId -> a -> b -> UGen) -> (a -> b -> m UGen)
liftU2 f a b = do n <- generateUId
                  return (f (UGenId n) a b)

liftU3 :: (UId m) => (UGenId -> a -> b -> c -> UGen) -> (a -> b -> c -> m UGen)
liftU3 f a b c = do n <- generateUId
                    return (f (UGenId n) a b c)

liftU4 :: (UId m) => (UGenId -> a -> b -> c -> d -> UGen) -> (a -> b -> c -> d -> m UGen)
liftU4 f a b c d = do n <- generateUId
                      return (f (UGenId n) a b c d)

-- | Construct proxied and multiple channel expanded UGen.
mkUGen :: Rate -> Name -> [UGen] -> [Output] -> Special -> UGenId -> UGen
mkUGen r n i o s z = proxy (mceExpand u)
    where u = UGen r n i o s z

-- | Oscillator constructor.
mkOscId :: UGenId -> Rate -> Name -> [UGen] -> Int -> UGen
mkOscId z r c i o = mkUGen r c i (replicate o r) (Special 0) z

-- | Oscillator constructor.
mkOsc :: Rate -> Name -> [UGen] -> Int -> UGen
mkOsc = mkOscId (UGenId 0)

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: UGenId -> Rate -> Name -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z r c i j o = mkOscId z r c (i ++ mceChannels j) o

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> Name -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mkOscMCEId (UGenId 0)

-- | Filter UGen constructor.
mkFilterId :: UGenId -> Name -> [UGen] -> Int -> UGen
mkFilterId z c i o = mkUGen r c i o' (Special 0) z
    where r = maximum (map rateOf i)
          o'= replicate o r

-- | Filter UGen constructor.
mkFilter :: Name -> [UGen] -> Int -> UGen
mkFilter = mkFilterId (UGenId 0)

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: Name -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o = mkUGen r c i o' (Special 0) (UGenId 0)
    where r = rateOf (i !! k)
          o' = replicate o r

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: Name -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE c i j o = mkFilter c (i ++ mceChannels j) o

-- | Operator UGen constructor.
mkOperator :: Name -> [UGen] -> Int -> UGen
mkOperator c i s = mkUGen r c i [r] (Special s) (UGenId 0)
    where r = maximum (map rateOf i)

-- UGen math.

-- | Unary math constructor with constant optimization.
uop :: Unary -> (Double -> Double) -> UGen -> UGen
uop _ f (Constant a) = Constant (f a)
uop i _ a = mkOperator "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimization.
binop :: Binary -> (Double -> Double -> Double) -> UGen -> UGen -> UGen
binop _ f (Constant a) (Constant b) = Constant (f a b)
binop i _ a b = mkOperator "BinaryOpUGen" [a, b] (fromEnum i)

instance Num UGen where
    negate         = uop Neg negate
    (+)            = binop Add (+)
    (-)            = binop Sub (-)
    (*)            = binop Mul (*)
    abs            = uop Abs abs
    signum         = uop Sign signum
    fromInteger a  = Constant (fromInteger a)

instance Fractional UGen where
    recip          = uop Recip recip
    (/)            = binop FDiv (/)
    fromRational a = Constant (fromRational a)

instance Floating UGen where
    pi             = Constant pi
    exp            = uop Exp exp
    log            = uop Log log
    sqrt           = uop Sqrt sqrt
    (**)           = binop Pow (**)
    logBase a b    = log b / log a
    sin            = uop Sin sin
    cos            = uop Cos cos
    tan            = uop Tan tan
    asin           = uop ArcSin asin
    acos           = uop ArcCos acos
    atan           = uop ArcTan atan
    sinh           = uop SinH sinh
    cosh           = uop CosH cosh
    tanh           = uop TanH tanh
    asinh x        = log (sqrt (x*x+1) + x)
    acosh x        = log (sqrt (x*x-1) + x)
    atanh x        = (log (1+x) - log (1-x)) / 2

instance Real UGen where
    toRational (Constant n) = toRational n
    toRational _ = error "toRational at non-constant UGen"

instance Integral UGen where
    quot = binop IDiv __binop
    rem = binop Mod __binop
    quotRem a b = (quot a b, rem a b)
    div = binop IDiv __binop
    mod = binop Mod __binop
    toInteger (Constant n) = floor n
    toInteger _ = error "toInteger at non-constant UGen"

instance Ord UGen where
    (Constant a) <  (Constant b) = a <  b
    _            <  _            = error "< at UGen is partial, see <*"
    (Constant a) <= (Constant b) = a <= b
    _            <= _            = error "<= at UGen is partial, see <=*"
    (Constant a) >  (Constant b) = a <  b
    _            >  _            = error "> at UGen is partial, see >*"
    (Constant a) >= (Constant b) = a >= b
    _            >= _            = error ">= at UGen is partial, see >=*"
    min  = binop Min min
    max  = binop Max max

instance Enum UGen where
    succ u                = u + 1
    pred u                = u - 1
    toEnum i              = Constant (fromIntegral i)
    fromEnum (Constant n) = truncate n
    fromEnum _            = error "cannot enumerate non-constant UGens"
    enumFrom              = iterate (+1)
    enumFromThen n m      = iterate (+(m-n)) n
    enumFromTo n m        = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m = takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')
        where p = if n' >= n then (>=) else (<=)

instance Random UGen where
    randomR (Constant l, Constant r) g = (Constant n, g') 
        where (n, g') = randomR (l,r) g
    randomR _                        _ = error "randomR: non constant (l,r)"
    random g = randomR (-1.0,1.0) g

instance EqE UGen where
    (==*)  = binop EQ (==*)
    (/=*)  = binop NE (/=*)

instance OrdE UGen where
    (<*)  = binop LT (<*)
    (<=*) = binop LE (<=*)
    (>*)  = binop GT (>*)
    (>=*) = binop GE (>=*)

instance UnaryOp UGen where
    notE           = uop Not notE
    isNil          = uop IsNil isNil
    notNil         = uop NotNil notNil
    bitNot         = uop BitNot bitNot
    asFloat        = uop AsFloat asFloat
    asInt          = uop AsInt asInt
    ceil           = uop Ceil ceil
    floorE         = uop Floor floorE
    frac           = uop Frac frac
    squared        = uop Squared squared
    cubed          = uop Cubed cubed
    midiCPS        = uop MIDICPS midiCPS
    cpsMIDI        = uop CPSMIDI cpsMIDI
    midiRatio      = uop MIDIRatio midiRatio
    ratioMIDI      = uop RatioMIDI ratioMIDI
    dbAmp          = uop DbAmp dbAmp
    ampDb          = uop AmpDb ampDb
    octCPS         = uop OctCPS octCPS
    cpsOct         = uop CPSOct cpsOct
    log2           = uop Log2 log2
    log10          = uop Log10 log10
    distort        = uop Distort distort
    softClip       = uop SoftClip softClip

instance BinaryOp UGen where
    iDiv           = binop IDiv __binop
    modE           = binop Mod __binop
    bitAnd         = binop BitAnd __binop
    bitOr          = binop BitOr __binop
    bitXOr         = binop BitXor __binop
    lcmE           = binop LCM __binop
    gcdE           = binop GCD __binop
    roundE         = binop Round __binop
    roundUp        = binop RoundUp __binop
    trunc          = binop Trunc __binop
    atan2E         = binop Atan2 __binop
    hypot          = binop Hypot __binop
    hypotx         = binop Hypotx __binop
    shiftLeft      = binop ShiftLeft __binop
    shiftRight     = binop ShiftRight __binop
    unsignedShift  = binop UnsignedShift __binop
    fill           = binop Fill __binop
    ring1          = binop Ring1 __binop
    ring2          = binop Ring2 __binop
    ring3          = binop Ring3 __binop
    ring4          = binop Ring4 __binop
    difSqr         = binop DifSqr __binop
    sumSqr         = binop SumSqr __binop
    sqrSum         = binop SqrSum __binop
    sqrDif         = binop SqrDif __binop
    absDif         = binop AbsDif __binop
    thresh         = binop Thresh __binop
    amClip         = binop AMClip __binop
    scaleNeg       = binop ScaleNeg __binop
    clip2          = binop Clip2 __binop
    excess         = binop Excess __binop
    fold2          = binop Fold2 __binop
    wrap2          = binop Wrap2 __binop
    firstArg       = binop FirstArg __binop
    randRange      = binop RandRange __binop
    exprandRange   = binop ExpRandRange __binop

instance TernaryOp UGen where
    wrap a b c = __ternaryop a b c
    fold a b c = __ternaryop a b c
    clip i l h = mkFilter "Clip" [i,l,h] 1

-- * Mix

-- | Collapse MCE by summing.
mix :: UGen -> UGen
mix (MCE u)  = foldl1 (+) u
mix u        = u

-- | Construct and sum a set of UGens.
mixFill :: Int -> (Int -> UGen) -> UGen
mixFill n f = mix (MCE (map f [0..n-1]))

-- | Clone UGen.
clone :: (UId m) => Int -> m UGen -> m UGen
clone n u = liftM MCE (replicateM n u)
