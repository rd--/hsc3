module Hsc.Math where

import Hsc.UGen
import Hsc.Construct

binop :: Special -> UGen -> UGen -> UGen
binop i a b = mkFilter "BinaryOpUGen" [a,b] 1 i (UId 0)

uop :: Special -> UGen -> UGen
uop   i a   = mkFilter "UnaryOpUGen"  [a]   1 i (UId 0)

instance Num UGen where
    negate         = uop 0
    (+)            = binop 0
    (-)            = binop 1
    (*)            = binop 2
    abs            = uop 5
    signum         = uop 11
    fromInteger a  = Constant (fromInteger a)

instance Fractional UGen where
    recip          = uop 16
    (/)            = binop 4
    fromRational a = Constant (fromRational a)

instance Floating UGen where
    pi             = Constant pi
    exp            = uop 15
    log            = uop 25
    sqrt           = uop 14
    (**)           = binop 25
    logBase a b    = log b / log a
    sin            = uop 28
    cos            = uop 29
    tan            = uop 30
    asin           = uop 31
    acos           = uop 32
    atan           = uop 33
    sinh           = uop 34
    cosh           = uop 35
    tanh           = uop 36
    -- cf. module Haskore.Interface.CSound.Orchestra
    asinh x        = log (sqrt (x*x+1) + x)
    acosh x        = log (sqrt (x*x-1) + x)
    atanh x        = (log (1+x) - log (1-x)) / 2

class (Num a, Fractional a, Floating a) => UnaryOp a where
    not            :: a -> a
    isnil          :: a -> a
    notnil         :: a -> a
    bitnot         :: a -> a
    asfloat        :: a -> a
    asint          :: a -> a
    ceil           :: a -> a
    floor          :: a -> a
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

instance UnaryOp UGen where
    not            = uop 1
    isnil          = uop 2
    notnil         = uop 3
    bitnot         = uop 4
    asfloat        = uop 6
    asint          = uop 7
    ceil           = uop 8
    floor          = uop 9
    frac           = uop 10
    squared        = uop 12
    cubed          = uop 13
    midicps        = uop 17
    cpsmidi        = uop 18
    midiratio      = uop 19
    ratiomidi      = uop 20
    dbamp          = uop 21
    ampdb          = uop 22
    octcps         = uop 23
    cpsoct         = uop 24
    log2           = uop 26
    log10          = uop 27

class EqU a where
    (==*)  :: a -> a -> a

instance EqU UGen where
    (==*)  = binop 6

class OrdU a where
    (<*)  :: a -> a -> a
    (<=*) :: a -> a -> a
    (>*)  :: a -> a -> a
    (>=*) :: a -> a -> a
    maxU  :: a -> a -> a
    minU  :: a -> a -> a

instance OrdU UGen where
    (<*)  = binop 8
    (<=*) = binop 10
    (>*)  = binop 9
    (>=*) = binop 11
    minU  = binop 12
    maxU  = binop 13

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
    pow            :: a -> a -> a
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

instance BinaryOp UGen where
    idiv           = binop  3
    mod            = binop  5
    bitand         = binop 14
    bitor          = binop 15
    bitxor         = binop 16
    lcm            = binop 17
    gcd            = binop 18
    round          = binop 19
    roundup        = binop 20
    trunc          = binop 21
    atan2          = binop 22
    hypot          = binop 23
    hypotx         = binop 24
    pow            = binop 25
    shiftleft      = binop 26
    shiftright     = binop 27
    unsignedshift  = binop 28
    fill           = binop 29
    ring1          = binop 30
    ring2          = binop 31
    ring3          = binop 32
    ring4          = binop 33
    difsqr         = binop 34
    sumsqr         = binop 35
    sqrsum         = binop 36
    sqrdif         = binop 37
    absdif         = binop 38
    thresh         = binop 39
    amclip         = binop 40
    scaleneg       = binop 41
    clip2          = binop 42
    excess         = binop 43
    fold2          = binop 44
    wrap2          = binop 45
    firstarg       = binop 46
    randrange      = binop 47
    exprandrange   = binop 48
