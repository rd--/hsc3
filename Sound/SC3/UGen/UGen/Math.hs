module Sound.SC3.UGen.UGen.Math () where

import System.Random
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct

-- | Unit generators are numbers.
instance Num UGen where
    negate         = mkUnaryOperator Neg negate
    (+)            = mkBinaryOperator Add (+)
    (-)            = mkBinaryOperator Sub (-)
    (*)            = mkBinaryOperator Mul (*)
    abs            = mkUnaryOperator Abs abs
    signum         = mkUnaryOperator Sign signum
    fromInteger    = Constant . fromInteger

-- | Unit generators are fractional.
instance Fractional UGen where
    recip          = mkUnaryOperator Recip recip
    (/)            = mkBinaryOperator FDiv (/)
    fromRational   = Constant . fromRational

-- | Unit generators are floating point.
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

-- | Unit generators are real.
instance Real UGen where
    toRational (Constant n) = toRational n
    toRational _ = error "toRational at non-constant UGen"

-- | Unit generators are integral.
instance Integral UGen where
    quot = mkBinaryOperator IDiv undefined
    rem = mkBinaryOperator Mod undefined
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator IDiv undefined
    mod = mkBinaryOperator Mod undefined
    toInteger (Constant n) = floor n
    toInteger _ = error "toInteger at non-constant UGen"

-- | Unit generators are orderable.
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

-- | Unit generators are enumerable.
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

-- | Unit generators are stochastic.
instance Random UGen where
    randomR (Constant l, Constant r) g = let (n, g') = randomR (l,r) g
                                         in (Constant n, g')
    randomR _ _ = error "randomR: non constant (l,r)"
    random g = randomR (-1.0, 1.0) g
