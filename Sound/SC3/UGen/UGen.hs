module Sound.SC3.UGen.UGen where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.Operator (Unary(..),Binary(..))
import Sound.SC3.UGen.Math

import Prelude hiding (EQ, GT, LT)
import Data.List (transpose)
import Data.Unique (newUnique, hashUnique)
import Control.Monad (liftM, replicateM)

type Name     = String
type Output   = Rate
type Special  = Int
data UId      = UId Int deriving (Eq, Show)
data UGen     = Constant Double
              | Control Rate Name Double
              | UGen Rate Name [UGen] [Output] Special UId
              | Proxy UGen Int
              | MCE [UGen]
              | MRG [UGen]
                deriving (Eq, Show)
data UType    = ConstantT | ControlT | UGenT | ProxyT | MCET | MRGT
                deriving (Eq, Show)

rateOf :: UGen -> Rate
rateOf (Constant _)        =  IR
rateOf (Control r _ _)     =  r
rateOf (UGen r _ _ _ _ _)  =  r
rateOf (Proxy u _)         =  rateOf u
rateOf (MCE u)             =  maximum $ map rateOf u
rateOf _                   =  error "rateOf: illegal ugen"

nodes :: UGen -> [UGen]
nodes u@(UGen _ _ i _ _ _) =  u : concatMap nodes i
nodes (Proxy u _)          =  u : nodes u
nodes (MCE u)              =  concatMap nodes u
nodes (MRG u)              =  concatMap nodes u
nodes u                    =  [u]

-- Apply depth first.

traverseu :: (UGen -> UGen) -> UGen -> UGen
traverseu f (UGen r n i o s uid) = f (UGen r n (map (traverseu f) i) o s uid)
traverseu f (MCE l)             = f (MCE (map (traverseu f) l))
traverseu f (MRG l)             = f (MRG (map (traverseu f) l))
traverseu f (Proxy u n)         = f (Proxy (traverseu f u) n)
traverseu f u                   = f u

utype :: UGen -> UType
utype (Constant _)          = ConstantT
utype (Control _ _ _)       = ControlT
utype (UGen _ _ _ _ _ _)    = UGenT
utype (Proxy _ _)           = ProxyT
utype (MCE _)               = MCET
utype (MRG _)               = MRGT

isConstant :: UGen -> Bool
isControl :: UGen -> Bool
isMCE :: UGen -> Bool
isMRG :: UGen -> Bool
isProxy :: UGen -> Bool
isUGen :: UGen -> Bool

isConstant u                = utype u == ConstantT
isControl u                 = utype u == ControlT
isUGen u                    = utype u == UGenT
isProxy u                   = utype u == ProxyT
isMCE u                     = utype u == MCET
isMRG u                     = utype u == MRGT

proxy :: UGen -> UGen
proxy (MCE l) = MCE (map proxy l)
proxy u@(UGen _ _ _ o _ _) =
   case o of
      (_:_:_) -> MCE (map (Proxy u) [0..(length o - 1)])
      _       -> u
proxy _       = error "proxy: illegal ugen"

-- MCE 

mceDegree :: UGen -> Int
mceDegree (MCE l) = length l
mceDegree _       = error "mceDegree: illegal ugen"

mceReq :: UGen -> Bool
mceReq (UGen _ _ i _ _ _) = not $ null $ filter isMCE i
mceReq _                  = False

mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n $ cycle l
mceExtend n u       = replicate n u

mceTransform :: UGen -> UGen
mceTransform (UGen r n i o s uid) = MCE (map f i')
    where f j = UGen r n j o s uid
          d   = maximum $ map mceDegree (filter isMCE i)
          i'  = transpose $ map (mceExtend d) i
mceTransform _                   = error "mceTransform: illegal ugen"

mced :: UGen -> UGen
mced u = if mceReq u then mceTransform u else u

-- Constructors

zeroUId :: UId
zeroUId = UId 0

mkId :: IO Int
mkId = liftM hashUnique newUnique

mkUId :: IO UId
mkUId = liftM UId mkId

uniquify :: UGen -> IO UGen
uniquify (UGen r n i o s _) = liftM (UGen r n i o s) mkUId
uniquify (MCE u)            = liftM MCE (mapM uniquify u)
uniquify u                  = error ("uniquify: illegal value" ++ show u)

consU :: Rate -> Name -> [UGen] -> [Output] -> Special -> UId -> UGen
consU r n i o s uid = proxy (mced u)
    where u = UGen r n i o s uid

mkOsc :: Rate -> Name -> [UGen] -> Int -> Special -> UGen
mkOsc r c i o s = consU r c i o' s zeroUId
    where o' = replicate o r

mkOsc' :: Rate -> Name -> [UGen] -> Int -> Special -> UId -> UGen
mkOsc' r c i o s uid = consU r c i o' s uid
    where o' = replicate o r

mkFilter :: Name -> [UGen] -> Int -> Special -> UGen
mkFilter c i o s = consU r c i o' s zeroUId
    where r = maximum (map rateOf i)
          o'= replicate o r

mcel :: UGen -> [UGen]
mcel (MCE l) = l
mcel u       = [u]

mkOscMCE :: Rate -> Name -> [UGen] -> UGen -> Int -> Special -> UGen
mkOscMCE r c i j o s = mkOsc r c (i ++ mcel j) o s

mkFilterMCE :: Name -> [UGen] -> UGen -> Int -> Special -> UGen
mkFilterMCE c i j o s = mkFilter c (i ++ mcel j) o s

-- Math instances.

uop :: Unary -> (Double -> Double) -> UGen -> UGen
uop _ f (Constant a) = Constant (f a) 
uop i _ a            = mkFilter "UnaryOpUGen"  [a]   1 (fromEnum i)

binop :: Binary -> (Double -> Double -> Double) -> UGen -> UGen -> UGen
binop _ f (Constant a) (Constant b) = Constant (f a b)
binop i _ a            b            = mkFilter "BinaryOpUGen" [a,b] 1 (fromEnum i)

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

instance Ord UGen where
    (<)  = error "Ord is partial, see OrdE"
    (<=) = error "Ord is partial, see OrdE"
    (>)  = error "Ord is partial, see OrdE"
    (>=) = error "Ord is partial, see OrdE"
    min  = binop Min min
    max  = binop Max max

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
    isnil          = uop IsNil isnil
    notnil         = uop NotNil notnil
    bitnot         = uop BitNot bitnot
    asfloat        = uop AsFloat asfloat
    asint          = uop AsInt asint
    ceil           = uop Ceil ceil
    floorE         = uop Floor floorE
    frac           = uop Frac frac
    squared        = uop Squared squared
    cubed          = uop Cubed cubed
    midicps        = uop MIDICPS midicps
    cpsmidi        = uop CPSMIDI cpsmidi
    midiratio      = uop MIDIRatio midiratio
    ratiomidi      = uop RatioMIDI ratiomidi
    dbamp          = uop DbAmp dbamp
    ampdb          = uop AmpDb ampdb
    octcps         = uop OctCPS octcps
    cpsoct         = uop CPSOct cpsoct
    log2           = uop Log2 log2
    log10          = uop Log10 log10
    softclip       = uop SoftClip softclip

instance BinaryOp UGen where
    idiv           = binop IDiv __binop
    mod            = binop Mod __binop
    bitand         = binop BitAnd __binop
    bitor          = binop BitOr __binop
    bitxor         = binop BitXor __binop
    lcm            = binop LCM __binop
    gcd            = binop GCD __binop
    round          = binop Round __binop
    roundup        = binop RoundUp __binop
    trunc          = binop Trunc __binop
    atan2          = binop Atan2 __binop
    hypot          = binop Hypot __binop
    hypotx         = binop Hypotx __binop
    shiftleft      = binop ShiftLeft __binop
    shiftright     = binop ShiftRight __binop
    unsignedshift  = binop UnsignedShift __binop
    fill           = binop Fill __binop
    ring1          = binop Ring1 __binop
    ring2          = binop Ring2 __binop
    ring3          = binop Ring3 __binop
    ring4          = binop Ring4 __binop
    difsqr         = binop DifSqr __binop
    sumsqr         = binop SumSqr __binop
    sqrsum         = binop SqrSum __binop
    sqrdif         = binop SqrDif __binop
    absdif         = binop AbsDif __binop
    thresh         = binop Thresh __binop
    amclip         = binop AMClip __binop
    scaleneg       = binop ScaleNeg __binop
    clip2          = binop Clip2 __binop
    excess         = binop Excess __binop
    fold2          = binop Fold2 __binop
    wrap2          = binop Wrap2 __binop
    firstarg       = binop FirstArg __binop
    randrange      = binop RandRange __binop
    exprandrange   = binop ExpRandRange __binop

instance TernaryOp UGen where
    wrap a b c = __ternaryop a b c
    fold a b c = __ternaryop a b c
    clip i l h = mkFilter "Clip" [i,l,h] 1 0

-- Mix

mix :: UGen -> UGen
mix (MCE u)  = foldl1 (+) u
mix u        = u

mix_fill :: Int -> (Int -> UGen) -> UGen
mix_fill n f = mix (MCE (map f [0..n-1]))

-- Duplicate

dupn :: Int -> UGen -> IO UGen
dupn  n u = liftM MCE (replicateM n (uniquify u))

dupn' :: Int -> IO UGen -> IO UGen
dupn' n u = u >>= dupn n

dup :: UGen -> IO UGen
dup       = dupn 2

dup' :: IO UGen -> IO UGen
dup'      = dupn' 2

