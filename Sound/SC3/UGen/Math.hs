module Sound.SC3.UGen.Math where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct
import Sound.SC3.UGen.UGen.Math ()

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

-- | Variant on Eq class, result is of the same type as the values compared.
class EqE a where
    (==*)  :: a -> a -> a
    (/=*)  :: a -> a -> a

instance EqE Double where
    a ==* b = if a == b then 1.0 else 0.0
    a /=* b = if a /= b then 1.0 else 0.0

instance EqE UGen where
    (==*)  = mkBinaryOperator EQ_ (==*)
    (/=*)  = mkBinaryOperator NE (/=*)

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

instance OrdE UGen where
    (<*)  = mkBinaryOperator LT_ (<*)
    (<=*) = mkBinaryOperator LE (<=*)
    (>*)  = mkBinaryOperator GT_ (>*)
    (>=*) = mkBinaryOperator GE (>=*)

-- | Unary operator class.
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

instance UnaryOp Double where
    notE a      = if a >  0.0 then 0.0 else 1.0
    isNil a     = if a == 0.0 then 0.0 else 1.0
    notNil a    = if a /= 0.0 then 0.0 else 1.0
    bitNot      = undefined
    asFloat     = undefined
    asInt       = undefined
    ceil a      = fromIntegral (ceiling a :: Integer)
    floorE a    = fromIntegral (floor a   :: Integer)
    frac        = undefined
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
    distort     = undefined
    softClip    = undefined

instance UnaryOp UGen where
    notE           = mkUnaryOperator Not notE
    isNil          = mkUnaryOperator IsNil isNil
    notNil         = mkUnaryOperator NotNil notNil
    bitNot         = mkUnaryOperator BitNot bitNot
    asFloat        = mkUnaryOperator AsFloat asFloat
    asInt          = mkUnaryOperator AsInt asInt
    ceil           = mkUnaryOperator Ceil ceil
    floorE         = mkUnaryOperator Floor floorE
    frac           = mkUnaryOperator Frac frac
    squared        = mkUnaryOperator Squared squared
    cubed          = mkUnaryOperator Cubed cubed
    midiCPS        = mkUnaryOperator MIDICPS midiCPS
    cpsMIDI        = mkUnaryOperator CPSMIDI cpsMIDI
    midiRatio      = mkUnaryOperator MIDIRatio midiRatio
    ratioMIDI      = mkUnaryOperator RatioMIDI ratioMIDI
    dbAmp          = mkUnaryOperator DbAmp dbAmp
    ampDb          = mkUnaryOperator AmpDb ampDb
    octCPS         = mkUnaryOperator OctCPS octCPS
    cpsOct         = mkUnaryOperator CPSOct cpsOct
    log2           = mkUnaryOperator Log2 log2
    log10          = mkUnaryOperator Log10 log10
    distort        = mkUnaryOperator Distort distort
    softClip       = mkUnaryOperator SoftClip softClip

-- | Binary operator class.
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

instance BinaryOp Double where
    iDiv               = undefined
    modE a b           = n - floorE n where n = a / b
    bitAnd             = undefined
    bitOr              = undefined
    bitXOr             = undefined
    lcmE               = undefined
    gcdE               = undefined
    roundE a b         = if b == 0 then a else floorE (a/b + 0.5) * b
    roundUp a b        = if b == 0 then a else ceil (a/b + 0.5) * b
    trunc              = undefined
    atan2E a b         = atan (b/a)
    hypot              = undefined
    hypotx             = undefined
    shiftLeft          = undefined
    shiftRight         = undefined
    unsignedShift      = undefined
    fill               = undefined
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
    clip2 a b          = clip_ a (-b) b
    excess a b         = a - clip_ a (-b) b
    fold2 a b          = fold a (-b) b
    wrap2 a b          = wrap a (-b) b
    firstArg a _       = a
    randRange          = undefined
    exprandRange       = undefined

instance BinaryOp UGen where
    iDiv           = mkBinaryOperator IDiv undefined
    modE           = mkBinaryOperator Mod modE
    bitAnd         = mkBinaryOperator BitAnd undefined
    bitOr          = mkBinaryOperator BitOr undefined
    bitXOr         = mkBinaryOperator BitXor undefined
    lcmE           = mkBinaryOperator LCM undefined
    gcdE           = mkBinaryOperator GCD undefined
    roundE         = mkBinaryOperator Round undefined
    roundUp        = mkBinaryOperator RoundUp undefined
    trunc          = mkBinaryOperator Trunc undefined
    atan2E         = mkBinaryOperator Atan2 undefined
    hypot          = mkBinaryOperator Hypot undefined
    hypotx         = mkBinaryOperator Hypotx undefined
    shiftLeft      = mkBinaryOperator ShiftLeft undefined
    shiftRight     = mkBinaryOperator ShiftRight undefined
    unsignedShift  = mkBinaryOperator UnsignedShift undefined
    fill           = mkBinaryOperator Fill undefined
    ring1          = mkBinaryOperator Ring1 undefined
    ring2          = mkBinaryOperator Ring2 undefined
    ring3          = mkBinaryOperator Ring3 undefined
    ring4          = mkBinaryOperator Ring4 undefined
    difSqr         = mkBinaryOperator DifSqr undefined
    sumSqr         = mkBinaryOperator SumSqr undefined
    sqrSum         = mkBinaryOperator SqrSum undefined
    sqrDif         = mkBinaryOperator SqrDif undefined
    absDif         = mkBinaryOperator AbsDif undefined
    thresh         = mkBinaryOperator Thresh undefined
    amClip         = mkBinaryOperator AMClip undefined
    scaleNeg       = mkBinaryOperator ScaleNeg undefined
    clip2          = mkBinaryOperator Clip2 undefined
    excess         = mkBinaryOperator Excess undefined
    fold2          = mkBinaryOperator Fold2 undefined
    wrap2          = mkBinaryOperator Wrap2 undefined
    firstArg       = mkBinaryOperator FirstArg undefined
    randRange      = mkBinaryOperator RandRange undefined
    exprandRange   = mkBinaryOperator ExpRandRange undefined

wrap :: (UnaryOp a, Ord a) => a -> a -> a -> a
wrap a b c = if a >= b && a <= c then a else a - r * floorE (a-b)/r 
        where r = c - b

fold :: (UnaryOp a, Ord a) => a -> a -> a -> a
fold a b c = if a >= b && a <= c then a else y' + b
    where r  = c - b
          r' = r + r
          x  = a - b
          y  = x - r' * floorE x/r'
          y' = if y >= r then r' - y else y

clip_ :: (Ord a) => a -> a -> a -> a
clip_ a b c = if a < b then b else if a > c then c else a
