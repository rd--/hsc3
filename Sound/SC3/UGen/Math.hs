-- | Non-standard mathematical classes and class instances.
module Sound.SC3.UGen.Math where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.UGen

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

-- | Variant on Eq class, result is of the same type as the values compared.
class EqE a where
    (==*) :: a -> a -> a
    (/=*) :: a -> a -> a

instance EqE Double where
    a ==* b = if a == b then 1.0 else 0.0
    a /=* b = if a /= b then 1.0 else 0.0

instance EqE UGen where
    (==*) = mkBinaryOperator EQ_ (==*)
    (/=*) = mkBinaryOperator NE (/=*)

-- | Variant on Ord class, result is of the same type as the values compared.
class OrdE a where
    (<*) :: a -> a -> a
    (<=*) :: a -> a -> a
    (>*) :: a -> a -> a
    (>=*) :: a -> a -> a

instance OrdE Double where
    a <* b = if a < b   then 1.0 else 0.0
    a <=* b = if a <= b  then 1.0 else 0.0
    a >* b = if a > b   then 1.0 else 0.0
    a >=* b = if a >= b  then 1.0 else 0.0

instance OrdE UGen where
    (<*) = mkBinaryOperator LT_ (<*)
    (<=*) = mkBinaryOperator LE (<=*)
    (>*) = mkBinaryOperator GT_ (>*)
    (>=*) = mkBinaryOperator GE (>=*)

-- | Unary operator class.
class (Floating a, Ord a) => UnaryOp a where
    ampDb :: a -> a
    ampDb a = log10 a * 20
    asFloat :: a -> a
    asFloat = undefined
    asInt :: a -> a
    asInt = undefined
    bitNot :: a -> a
    bitNot = undefined
    ceil :: a -> a
    cpsMIDI :: a -> a
    cpsMIDI a = (log2 (a * (1.0 / 440.0)) * 12.0) + 69.0
    cpsOct :: a -> a
    cpsOct a = log2 (a * (1.0 / 440.0)) + 4.75
    cubed :: a -> a
    cubed   a = a * a * a
    dbAmp :: a -> a
    dbAmp a = 10 ** (a * 0.05)
    distort :: a -> a
    distort = undefined
    floorE :: a -> a
    frac :: a -> a
    frac = undefined
    isNil :: a -> a
    isNil a = if a == 0.0 then 0.0 else 1.0
    log10 :: a -> a
    log10 = logBase 10
    log2 :: a -> a
    log2 = logBase 2
    midiCPS :: a -> a
    midiCPS a = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))
    midiRatio :: a -> a
    midiRatio a = 2.0 ** (a * (1.0 / 12.0))
    notE :: a -> a
    notE a = if a >  0.0 then 0.0 else 1.0
    notNil :: a -> a
    notNil a = if a /= 0.0 then 0.0 else 1.0
    octCPS :: a -> a
    octCPS a = 440.0 * (2.0 ** (a - 4.75))
    ramp_ :: a -> a
    ramp_ _ = undefined
    ratioMIDI :: a -> a
    ratioMIDI a = 12.0 * log2 a
    softClip :: a -> a
    softClip = undefined
    squared :: a -> a
    squared a = a * a

instance UnaryOp Double where
    ceil a = fromIntegral (ceiling a :: Integer)
    floorE a = fromIntegral (floor a :: Integer)

instance UnaryOp UGen where
    ampDb = mkUnaryOperator AmpDb ampDb
    asFloat = mkUnaryOperator AsFloat asFloat
    asInt = mkUnaryOperator AsInt asInt
    bitNot = mkUnaryOperator BitNot bitNot
    ceil = mkUnaryOperator Ceil ceil
    cpsMIDI = mkUnaryOperator CPSMIDI cpsMIDI
    cpsOct = mkUnaryOperator CPSOct cpsOct
    cubed = mkUnaryOperator Cubed cubed
    dbAmp = mkUnaryOperator DbAmp dbAmp
    distort = mkUnaryOperator Distort distort
    floorE = mkUnaryOperator Floor floorE
    frac = mkUnaryOperator Frac frac
    isNil = mkUnaryOperator IsNil isNil
    log10 = mkUnaryOperator Log10 log10
    log2 = mkUnaryOperator Log2 log2
    midiCPS = mkUnaryOperator MIDICPS midiCPS
    midiRatio = mkUnaryOperator MIDIRatio midiRatio
    notE = mkUnaryOperator Not notE
    notNil = mkUnaryOperator NotNil notNil
    octCPS = mkUnaryOperator OctCPS octCPS
    ramp_ = mkUnaryOperator Ramp ramp_
    ratioMIDI = mkUnaryOperator RatioMIDI ratioMIDI
    softClip = mkUnaryOperator SoftClip softClip
    squared = mkUnaryOperator Squared squared

-- | Binary operator class.
class (Floating a, Ord a) => BinaryOp a where
    absDif :: a -> a -> a
    absDif a b = abs (a - b)
    amClip :: a -> a -> a
    amClip a b = if b <= 0 then 0 else a * b
    atan2E :: a -> a -> a
    atan2E a b = atan (b/a)
    bitAnd :: a -> a -> a
    bitAnd = undefined
    bitOr :: a -> a -> a
    bitOr = undefined
    bitXOr :: a -> a -> a
    bitXOr = undefined
    clip2 :: a -> a -> a
    clip2 a b = clip_ a (-b) b
    difSqr :: a -> a -> a
    difSqr a b = (a*a) - (b*b)
    excess :: a -> a -> a
    excess a b = a - clip_ a (-b) b
    exprandRange :: a -> a -> a
    exprandRange = undefined
    fill :: a -> a -> a
    fill = undefined
    firstArg :: a -> a -> a
    firstArg a _ = a
    fold2 :: a -> a -> a
    gcdE :: a -> a -> a
    gcdE = undefined
    hypot :: a -> a -> a
    hypot = undefined
    hypotx :: a -> a -> a
    hypotx = undefined
    iDiv :: a -> a -> a
    iDiv = undefined
    lcmE :: a -> a -> a
    lcmE = undefined
    modE :: a -> a -> a
    randRange :: a -> a -> a
    randRange = undefined
    ring1 :: a -> a -> a
    ring1 a b = a * b + a
    ring2 :: a -> a -> a
    ring2 a b = a * b + a + b
    ring3 :: a -> a -> a
    ring3 a b = a * a * b
    ring4 :: a -> a -> a
    ring4 a b = a * a * b - a * b * b
    roundE :: a -> a -> a
    roundUp :: a -> a -> a
    scaleNeg :: a -> a -> a
    scaleNeg a b = (abs a - a) * b' + a where b' = 0.5 * b + 0.5
    shiftLeft :: a -> a -> a
    shiftLeft = undefined
    shiftRight :: a -> a -> a
    shiftRight = undefined
    sqrDif :: a -> a -> a
    sqrDif a b = (a-b) * (a-b)
    sqrSum :: a -> a -> a
    sqrSum a b = (a+b) * (a+b)
    sumSqr :: a -> a -> a
    sumSqr a b = (a*a) + (b*b)
    thresh :: a -> a -> a
    thresh a b = if a <  b then 0 else a
    trunc :: a -> a -> a
    trunc = undefined
    unsignedShift :: a -> a -> a
    unsignedShift = undefined
    wrap2 :: a -> a -> a

instance BinaryOp Double where
    fold2 a b = fold_ a (-b) b
    modE a b = let n = a / b in n - floorE n
    roundE a b = if b == 0 then a else floorE (a/b + 0.5) * b
    roundUp a b = if b == 0 then a else ceil (a/b + 0.5) * b
    wrap2 a b = wrap_ a (-b) b

instance BinaryOp UGen where
    iDiv = mkBinaryOperator IDiv undefined
    modE = mkBinaryOperator Mod modE
    bitAnd = mkBinaryOperator BitAnd undefined
    bitOr = mkBinaryOperator BitOr undefined
    bitXOr = mkBinaryOperator BitXor undefined
    lcmE = mkBinaryOperator LCM undefined
    gcdE = mkBinaryOperator GCD undefined
    roundE = mkBinaryOperator Round undefined
    roundUp = mkBinaryOperator RoundUp undefined
    trunc = mkBinaryOperator Trunc undefined
    atan2E = mkBinaryOperator Atan2 undefined
    hypot = mkBinaryOperator Hypot undefined
    hypotx = mkBinaryOperator Hypotx undefined
    shiftLeft = mkBinaryOperator ShiftLeft undefined
    shiftRight = mkBinaryOperator ShiftRight undefined
    unsignedShift = mkBinaryOperator UnsignedShift undefined
    fill = mkBinaryOperator Fill undefined
    ring1 = mkBinaryOperator Ring1 undefined
    ring2 = mkBinaryOperator Ring2 undefined
    ring3 = mkBinaryOperator Ring3 undefined
    ring4 = mkBinaryOperator Ring4 undefined
    difSqr = mkBinaryOperator DifSqr undefined
    sumSqr = mkBinaryOperator SumSqr undefined
    sqrSum = mkBinaryOperator SqrSum undefined
    sqrDif = mkBinaryOperator SqrDif undefined
    absDif = mkBinaryOperator AbsDif undefined
    thresh = mkBinaryOperator Thresh undefined
    amClip = mkBinaryOperator AMClip undefined
    scaleNeg = mkBinaryOperator ScaleNeg undefined
    clip2 = mkBinaryOperator Clip2 undefined
    excess = mkBinaryOperator Excess undefined
    fold2 = mkBinaryOperator Fold2 undefined
    wrap2 = mkBinaryOperator Wrap2 undefined
    firstArg = mkBinaryOperator FirstArg undefined
    randRange = mkBinaryOperator RandRange undefined
    exprandRange = mkBinaryOperator ExpRandRange undefined

wrap_ :: (UnaryOp a, Ord a) => a -> a -> a -> a
wrap_ a b c =
    let r = c - b
    in if a >= b && a <= c then a else a - r * floorE (a-b)/r

fold_ :: (UnaryOp a, Ord a) => a -> a -> a -> a
fold_ a b c =
    let r = c - b
        r' = r + r
        x = a - b
        y = x - r' * floorE x/r'
        y' = if y >= r then r' - y else y
    in if a >= b && a <= c then a else y' + b

clip_ :: (Ord a) => a -> a -> a -> a
clip_ a b c = if a < b then b else if a > c then c else a
