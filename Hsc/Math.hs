module Hsc.Math where

import Hsc.UGen

binop :: Special -> UGen -> UGen -> UGen
binop i a b =  UGen r "BinaryOpUGen" [a,b] [r] i 0 
    where r = max (rate a) (rate b)

uop :: Special -> UGen -> UGen 
uop   i a   =  UGen r "UnaryOpUGen"  [a]   [r] i 0 
    where r = rate a

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
    pi             = Constant 3.141592653589793
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
    asinh a        = Constant 0
    acosh a        = Constant 0
    atanh a        = Constant 0

class (Num a, Fractional a, Floating a) => Op a where
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

instance Op UGen where
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

