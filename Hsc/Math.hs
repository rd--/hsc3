module Hsc.Math where

import Prelude hiding (EQ, GT, LT)
import Hsc.Operator
import Hsc.UGen (UGen(Constant, MCE))
import Hsc.Construct (mkFilter)

uop _ f (Constant a) = Constant (f a) 
uop i _ a            = mkFilter "UnaryOpUGen"  [a]   1 (fromEnum (i :: Unary))

binop _ f (Constant a) (Constant b) = Constant (f a b)
binop i _ a            b            = mkFilter "BinaryOpUGen" [a,b] 1 (fromEnum (i :: Binary))

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

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

instance Ord UGen where
    (<)  = error "Ord is partial, see OrdE"
    (<=) = error "Ord is partial, see OrdE"
    (>)  = error "Ord is partial, see OrdE"
    (>=) = error "Ord is partial, see OrdE"
    min  = binop Min min
    max  = binop Max max

class EqE a where
    (==*)  :: a -> a -> a
    (/=*)  :: a -> a -> a

instance EqE UGen where
    (==*)  = binop EQ (==*)
    (/=*)  = binop NE (/=*)

instance EqE Double where
    a ==* b = if a == b then 1.0 else 0.0
    a /=* b = if a /= b then 1.0 else 0.0

class OrdE a where
    (<*)  :: a -> a -> a
    (<=*) :: a -> a -> a
    (>*)  :: a -> a -> a
    (>=*) :: a -> a -> a

instance OrdE UGen where
    (<*)  = binop LT (<*)
    (<=*) = binop LE (<=*)
    (>*)  = binop GT (>*)
    (>=*) = binop GE (>=*)

instance OrdE Double where
    a <* b   = if a < b   then 1.0 else 0.0
    a <=* b  = if a <= b  then 1.0 else 0.0
    a >* b   = if a > b   then 1.0 else 0.0
    a >=* b  = if a >= b  then 1.0 else 0.0

class (Num a, Fractional a, Floating a) => UnaryOp a where
    notE           :: a -> a
    isnil          :: a -> a
    notnil         :: a -> a
    bitnot         :: a -> a
    asfloat        :: a -> a
    asint          :: a -> a
    ceil           :: a -> a
    floorE         :: a -> a
    frac           :: a -> a
    squared        :: a -> a
    cubed          :: a -> a
    midicps        :: a -> a
    cpsmidi        :: a -> a
    midiratio      :: a -> a
    ratiomidi      :: a -> a
    dbamp          :: a -> a
    ampdb          :: a -> a
    octcps         :: a -> a
    cpsoct         :: a -> a
    log2           :: a -> a
    log10          :: a -> a
    softclip       :: a -> a

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

__uop a = error ("unimplemented unary op" ++ show a)

instance UnaryOp Double where
    notE a      = if a >  0.0 then 0.0 else 1.0
    isnil a     = if a == 0.0 then 0.0 else 1.0
    notnil a    = if a /= 0.0 then 0.0 else 1.0
    bitnot a    = __uop a
    asfloat a   = __uop a
    asint a     = __uop a
    ceil a      = fromIntegral (ceiling a)
    floorE a    = fromIntegral (floor a)
    frac a      = __uop a
    squared a   = a * a
    cubed   a   = a * a * a
    midicps a   = 440.0 * (2.0 ** ((a - 69.0) * 0.083333333333))
    cpsmidi a   = (log2 (a * 0.0022727272727) * 12.0) + 69.0
    midiratio a = 2.0 ** (a * 0.083333333333)
    ratiomidi a = 12.0 * (log2 a)
    dbamp a     = __uop a
    ampdb a     = __uop a
    octcps a    = 440.0 * (2.0 ** (a - 4.75))
    cpsoct a    = log2 (a * 0.0022727272727) + 4.75
    log2 a      = logBase 2 a
    log10 a     = logBase 10 a
    softclip a  = __uop a

class (Num a, Fractional a, Floating a) => BinaryOp a where
    idiv           :: a -> a -> a
    mod            :: a -> a -> a
    bitand         :: a -> a -> a
    bitor          :: a -> a -> a
    bitxor         :: a -> a -> a
    lcm            :: a -> a -> a
    gcd            :: a -> a -> a
    round          :: a -> a -> a
    roundup        :: a -> a -> a
    trunc          :: a -> a -> a
    atan2          :: a -> a -> a
    hypot          :: a -> a -> a
    hypotx         :: a -> a -> a
    shiftleft      :: a -> a -> a
    shiftright     :: a -> a -> a
    unsignedshift  :: a -> a -> a
    fill           :: a -> a -> a
    ring1          :: a -> a -> a
    ring2          :: a -> a -> a
    ring3          :: a -> a -> a
    ring4          :: a -> a -> a
    difsqr         :: a -> a -> a
    sumsqr         :: a -> a -> a
    sqrdif         :: a -> a -> a
    sqrsum         :: a -> a -> a
    absdif         :: a -> a -> a
    thresh         :: a -> a -> a
    amclip         :: a -> a -> a
    scaleneg       :: a -> a -> a
    clip2          :: a -> a -> a
    excess         :: a -> a -> a
    fold2          :: a -> a -> a
    wrap2          :: a -> a -> a
    firstarg       :: a -> a -> a
    randrange      :: a -> a -> a
    exprandrange   :: a -> a -> a

__binop a b = error ("unimplemented binop" ++ show (a,b))

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

instance BinaryOp Double where
    idiv a b           = __binop a b
    mod a b            = __binop a b
    bitand a b         = __binop a b
    bitor a b          = __binop a b
    bitxor a b         = __binop a b
    lcm a b            = __binop a b
    gcd a b            = __binop a b
    round a b          = if b == 0 then a else floorE (a/b + 0.5) * b
    roundup a b        = if b == 0 then a else ceil (a/b + 0.5) * b
    trunc a b          = __binop a b
    atan2 a b          = atan (b/a)
    hypot a b          = __binop a b
    hypotx a b         = __binop a b
    shiftleft a b      = __binop a b
    shiftright a b     = __binop a b
    unsignedshift a b  = __binop a b
    fill a b           = __binop a b
    ring1 a b          = a * b + a
    ring2 a b          = a * b + a + b
    ring3 a b          = a * a * b
    ring4 a b          = a * a * b - a * b * b
    difsqr a b         = (a*a) - (b*b)
    sumsqr a b         = (a*a) + (b*b)
    sqrsum a b         = (a+b) * (a+b)
    sqrdif a b         = (a-b) * (a-b)
    absdif a b         = abs (a - b)
    thresh a b         = if a <  b then 0 else a
    amclip a b         = if b <= 0 then 0 else a * b
    scaleneg a b       = (abs a - a) * b' + a where b' = 0.5 * b + 0.5
    clip2 a b          = clip a (-b) b
    excess a b         = a - clip a (-b) b
    fold2 a b          = fold a (-b) b
    wrap2 a b          = wrap a (-b) b
    firstarg a _       = a
    randrange a b      = __binop a b
    exprandrange a b   = __binop a b

class (Num a, Fractional a, Floating a) => TernaryOp a where
    wrap :: a -> a -> a -> a
    fold :: a -> a -> a -> a
    clip :: a -> a -> a -> a

instance TernaryOp Double where
    wrap a b c = if a >= b && a <= c 
                 then a 
                 else a - r * floorE (a-b)/r 
        where r = c - b
    fold a b c = if a >= b && a <= c 
                 then a 
                 else y' + b
        where r  = c - b
              r' = r + r
              x  = a - b
              y  = x - r' * floorE x/r'
              y' = if y >= r then r' - y else y
    clip a b c = if a < b then b else if a > c then c else a

__ternaryop a b c = error ("unimplemented ternary op" ++ show (a, b, c))

instance TernaryOp UGen where
    wrap a b c = __ternaryop a b c
    fold a b c = __ternaryop a b c
    clip i l h = mkFilter "Clip" [i,l,h] 1 0

mix (MCE u)  = foldl1 (+) u
mix u        = u

mix_fill n f = mix (MCE (map f [0..n-1]))
