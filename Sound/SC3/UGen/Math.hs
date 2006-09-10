module Sound.SC3.UGen.Math where

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

class EqE a where
    (==*)  :: a -> a -> a
    (/=*)  :: a -> a -> a

instance EqE Double where
    a ==* b = if a == b then 1.0 else 0.0
    a /=* b = if a /= b then 1.0 else 0.0

class OrdE a where
    (<*)  :: a -> a -> a
    (<=*) :: a -> a -> a
    (>*)  :: a -> a -> a
    (>=*) :: a -> a -> a

instance OrdE Double where
    a <* b   = if a < b   then 1.0 else 0.0
    a <=* b  = if a <= b  then 1.0 else 0.0
    a >* b   = if a > b   then 1.0 else 0.0
    a >=* b  = if a >= b  then 1.0 else 0.0

class (Floating a) => UnaryOp a where
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

__uop :: Show a => a -> a
__uop a = error ("unimplemented unary op" ++ show a)

instance UnaryOp Double where
    notE a      = if a >  0.0 then 0.0 else 1.0
    isnil a     = if a == 0.0 then 0.0 else 1.0
    notnil a    = if a /= 0.0 then 0.0 else 1.0
    bitnot a    = __uop a
    asfloat a   = __uop a
    asint a     = __uop a
    ceil a      = fromIntegral (ceiling a :: Integer)
    floorE a    = fromIntegral (floor a   :: Integer)
    frac a      = __uop a
    squared a   = a * a
    cubed   a   = a * a * a
    midicps a   = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))
    cpsmidi a   = (log2 (a * (1.0 / 440.0)) * 12.0) + 69.0
    midiratio a = 2.0 ** (a * (1.0 / 12.0))
    ratiomidi a = 12.0 * (log2 a)
    dbamp a     = __uop a
    ampdb a     = __uop a
    octcps a    = 440.0 * (2.0 ** (a - 4.75))
    cpsoct a    = log2 (a * (1.0 / 440.0)) + 4.75
    log2 a      = logBase 2 a
    log10 a     = logBase 10 a
    softclip a  = __uop a

class (Floating a) => BinaryOp a where
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

__binop :: Show a => a -> a -> a
__binop a b = error ("unimplemented binop" ++ show (a,b))

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

class (Floating a) => TernaryOp a where
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

__ternaryop :: Show a => a -> a -> a -> a
__ternaryop a b c = error ("unimplemented ternary op" ++ show (a, b, c))

