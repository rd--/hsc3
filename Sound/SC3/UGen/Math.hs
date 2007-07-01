module Sound.SC3.UGen.Math where

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

-- | Variant on Eq class, result is of the same type as the values compared.
class EqE a where
    (==*)  :: a -> a -> a
    (/=*)  :: a -> a -> a

instance EqE Double where
    a ==* b = if a == b then 1.0 else 0.0
    a /=* b = if a /= b then 1.0 else 0.0

-- | Variant on Ord class, result is of the same type as the values compared.
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
    isNil          :: a -> a
    notNil         :: a -> a
    bitNot         :: a -> a
    asFloat        :: a -> a
    asInt          :: a -> a
    ceil           :: a -> a
    floorE         :: a -> a
    frac           :: a -> a
    squared        :: a -> a
    cubed          :: a -> a
    midiCPS        :: a -> a
    cpsMIDI        :: a -> a
    midiRatio      :: a -> a
    ratioMIDI      :: a -> a
    dbAmp          :: a -> a
    ampDb          :: a -> a
    octCPS         :: a -> a
    cpsOct         :: a -> a
    log2           :: a -> a
    log10          :: a -> a
    distort        :: a -> a
    softClip       :: a -> a

__uop :: Show a => a -> a
__uop a = error ("unimplemented unary op" ++ show a)

instance UnaryOp Double where
    notE a      = if a >  0.0 then 0.0 else 1.0
    isNil a     = if a == 0.0 then 0.0 else 1.0
    notNil a    = if a /= 0.0 then 0.0 else 1.0
    bitNot a    = __uop a
    asFloat a   = __uop a
    asInt a     = __uop a
    ceil a      = fromIntegral (ceiling a :: Integer)
    floorE a    = fromIntegral (floor a   :: Integer)
    frac a      = __uop a
    squared a   = a * a
    cubed   a   = a * a * a
    midiCPS a   = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))
    cpsMIDI a   = (log2 (a * (1.0 / 440.0)) * 12.0) + 69.0
    midiRatio a = 2.0 ** (a * (1.0 / 12.0))
    ratioMIDI a = 12.0 * (log2 a)
    dbAmp a     = 10 ** (a * 0.05)
    ampDb a     = (log10 a) * 20
    octCPS a    = 440.0 * (2.0 ** (a - 4.75))
    cpsOct a    = log2 (a * (1.0 / 440.0)) + 4.75
    log2 a      = logBase 2 a
    log10 a     = logBase 10 a
    distort a   = __uop a
    softClip a  = __uop a

class (Floating a) => BinaryOp a where
    iDiv           :: a -> a -> a
    modE           :: a -> a -> a
    bitAnd         :: a -> a -> a
    bitOr          :: a -> a -> a
    bitXOr         :: a -> a -> a
    lcmE           :: a -> a -> a
    gcdE           :: a -> a -> a
    roundE         :: a -> a -> a
    roundUp        :: a -> a -> a
    trunc          :: a -> a -> a
    atan2E         :: a -> a -> a
    hypot          :: a -> a -> a
    hypotx         :: a -> a -> a
    shiftLeft      :: a -> a -> a
    shiftRight     :: a -> a -> a
    unsignedShift  :: a -> a -> a
    fill           :: a -> a -> a
    ring1          :: a -> a -> a
    ring2          :: a -> a -> a
    ring3          :: a -> a -> a
    ring4          :: a -> a -> a
    difSqr         :: a -> a -> a
    sumSqr         :: a -> a -> a
    sqrDif         :: a -> a -> a
    sqrSum         :: a -> a -> a
    absDif         :: a -> a -> a
    thresh         :: a -> a -> a
    amClip         :: a -> a -> a
    scaleNeg       :: a -> a -> a
    clip2          :: a -> a -> a
    excess         :: a -> a -> a
    fold2          :: a -> a -> a
    wrap2          :: a -> a -> a
    firstArg       :: a -> a -> a
    randRange      :: a -> a -> a
    exprandRange   :: a -> a -> a

__binop :: Show a => a -> a -> a
__binop a b = error ("unimplemented binop" ++ show (a,b))

instance BinaryOp Double where
    iDiv a b           = __binop a b
    modE a b           = __binop a b
    bitAnd a b         = __binop a b
    bitOr a b          = __binop a b
    bitXOr a b         = __binop a b
    lcmE a b           = __binop a b
    gcdE a b           = __binop a b
    roundE a b         = if b == 0 then a else floorE (a/b + 0.5) * b
    roundUp a b        = if b == 0 then a else ceil (a/b + 0.5) * b
    trunc a b          = __binop a b
    atan2E a b         = atan (b/a)
    hypot a b          = __binop a b
    hypotx a b         = __binop a b
    shiftLeft a b      = __binop a b
    shiftRight a b     = __binop a b
    unsignedShift a b  = __binop a b
    fill a b           = __binop a b
    ring1 a b          = a * b + a
    ring2 a b          = a * b + a + b
    ring3 a b          = a * a * b
    ring4 a b          = a * a * b - a * b * b
    difSqr a b         = (a*a) - (b*b)
    sumSqr a b         = (a*a) + (b*b)
    sqrSum a b         = (a+b) * (a+b)
    sqrDif a b         = (a-b) * (a-b)
    absDif a b         = abs (a - b)
    thresh a b         = if a <  b then 0 else a
    amClip a b         = if b <= 0 then 0 else a * b
    scaleNeg a b       = (abs a - a) * b' + a where b' = 0.5 * b + 0.5
    clip2 a b          = clip a (-b) b
    excess a b         = a - clip a (-b) b
    fold2 a b          = fold a (-b) b
    wrap2 a b          = wrap a (-b) b
    firstArg a _       = a
    randRange a b      = __binop a b
    exprandRange a b   = __binop a b

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

