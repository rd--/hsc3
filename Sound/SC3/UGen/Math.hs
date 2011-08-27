-- | Non-standard mathematical classes and class instances.
module Sound.SC3.UGen.Math where

import qualified Foreign.C.Math.Double as M
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

class RealFracE a where
  properFractionE :: a -> (a,a)
  truncateE :: a -> a
  roundE :: a -> a
  ceilingE :: a -> a
  floorE :: a -> a

truncatef :: RealFrac a => a -> a
truncatef a = fromIntegral (truncate a :: Integer)

roundf :: RealFrac a => a -> a
roundf a = fromIntegral (round a :: Integer)

ceilingf :: RealFrac a => a -> a
ceilingf a = fromIntegral (ceiling a :: Integer)

floorf :: RealFrac a => a -> a
floorf a = fromIntegral (floor a :: Integer)

ftruncate :: Double -> Double
ftruncate = M.trunc

fround :: Double -> Double
fround = M.round

fceiling :: Double -> Double
fceiling = M.ceil

ffloor :: Double -> Double
ffloor = M.floor

instance RealFracE Double where
    properFractionE n =
        let (i,j) = properFraction n
        in (fromIntegral (i::Integer),j)
    truncateE = ftruncate
    roundE = fround
    ceilingE = fceiling
    floorE = ffloor

roundTo_ :: Double -> Double -> Double
roundTo_ a b = if b == 0 then a else ffloor (a/b + 0.5) * b

roundTo :: UGen -> UGen -> UGen
roundTo = mkBinaryOperator Round roundTo_

instance RealFracE UGen where
    properFractionE = error "RealFracE,UGen,partial"
    truncateE = error "RealFracE,UGen,partial"
    roundE i = roundTo i 1
    ceilingE = mkUnaryOperator Ceil fceiling
    floorE = mkUnaryOperator Floor ffloor

ceil :: UGen -> UGen
ceil = ceilingE

midiCPS' :: Floating a => a -> a
midiCPS' i = 440.0 * (2.0 ** ((i - 69.0) * (1.0 / 12.0)))

-- | Unary operator class.
class (Floating a, Ord a) => UnaryOp a where
    ampDb :: a -> a
    ampDb a = log10 a * 20
    asFloat :: a -> a
    asFloat = error "asFloat"
    asInt :: a -> a
    asInt = error "asInt"
    bitNot :: a -> a
    bitNot = error "bitNot"
    cpsMIDI :: a -> a
    cpsMIDI a = (log2 (a * (1.0 / 440.0)) * 12.0) + 69.0
    cpsOct :: a -> a
    cpsOct a = log2 (a * (1.0 / 440.0)) + 4.75
    cubed :: a -> a
    cubed   a = a * a * a
    dbAmp :: a -> a
    dbAmp a = 10 ** (a * 0.05)
    distort :: a -> a
    distort = error "distort"
    frac :: a -> a
    frac = error "frac"
    isNil :: a -> a
    isNil a = if a == 0.0 then 0.0 else 1.0
    log10 :: a -> a
    log10 = logBase 10
    log2 :: a -> a
    log2 = logBase 2
    midiCPS :: a -> a
    midiCPS = midiCPS'
    midiRatio :: a -> a
    midiRatio a = 2.0 ** (a * (1.0 / 12.0))
    notE :: a -> a
    notE a = if a >  0.0 then 0.0 else 1.0
    notNil :: a -> a
    notNil a = if a /= 0.0 then 0.0 else 1.0
    octCPS :: a -> a
    octCPS a = 440.0 * (2.0 ** (a - 4.75))
    ramp_ :: a -> a
    ramp_ _ = error "ramp_"
    ratioMIDI :: a -> a
    ratioMIDI a = 12.0 * log2 a
    softClip :: a -> a
    softClip = error "softClip"
    squared :: a -> a
    squared a = a * a

instance UnaryOp Double where

instance UnaryOp UGen where
    ampDb = mkUnaryOperator AmpDb ampDb
    asFloat = mkUnaryOperator AsFloat asFloat
    asInt = mkUnaryOperator AsInt asInt
    bitNot = mkUnaryOperator BitNot bitNot
    cpsMIDI = mkUnaryOperator CPSMIDI cpsMIDI
    cpsOct = mkUnaryOperator CPSOct cpsOct
    cubed = mkUnaryOperator Cubed cubed
    dbAmp = mkUnaryOperator DbAmp dbAmp
    distort = mkUnaryOperator Distort distort
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
    bitAnd = error "bitAnd"
    bitOr :: a -> a -> a
    bitOr = error "bitOr"
    bitXOr :: a -> a -> a
    bitXOr = error "bitXOr"
    clip2 :: a -> a -> a
    clip2 a b = clip_ a (-b) b
    difSqr :: a -> a -> a
    difSqr a b = (a*a) - (b*b)
    excess :: a -> a -> a
    excess a b = a - clip_ a (-b) b
    exprandRange :: a -> a -> a
    exprandRange = error "exprandRange"
    fill :: a -> a -> a
    fill = error "fill"
    firstArg :: a -> a -> a
    firstArg a _ = a
    fold2 :: a -> a -> a
    gcdE :: a -> a -> a
    gcdE = error "gcdE"
    hypot :: a -> a -> a
    hypot = error "hypot"
    hypotx :: a -> a -> a
    hypotx = error "hypotx"
    iDiv :: a -> a -> a
    iDiv = error "iDiv"
    lcmE :: a -> a -> a
    lcmE = error "lcmE"
    modE :: a -> a -> a
    randRange :: a -> a -> a
    randRange = error "randRange"
    ring1 :: a -> a -> a
    ring1 a b = a * b + a
    ring2 :: a -> a -> a
    ring2 a b = a * b + a + b
    ring3 :: a -> a -> a
    ring3 a b = a * a * b
    ring4 :: a -> a -> a
    ring4 a b = a * a * b - a * b * b
    roundUp :: a -> a -> a
    scaleNeg :: a -> a -> a
    scaleNeg a b = (abs a - a) * b' + a where b' = 0.5 * b + 0.5
    shiftLeft :: a -> a -> a
    shiftLeft = error "shiftLeft"
    shiftRight :: a -> a -> a
    shiftRight = error "shiftRight"
    sqrDif :: a -> a -> a
    sqrDif a b = (a-b) * (a-b)
    sqrSum :: a -> a -> a
    sqrSum a b = (a+b) * (a+b)
    sumSqr :: a -> a -> a
    sumSqr a b = (a*a) + (b*b)
    thresh :: a -> a -> a
    thresh a b = if a <  b then 0 else a
    trunc :: a -> a -> a
    trunc = error "trunc"
    unsignedShift :: a -> a -> a
    unsignedShift = error "unsignedShift"
    wrap2 :: a -> a -> a

-- SC3 % does not return negative numbers.
fmod :: Double -> Double -> Double
fmod i j =
    let k = i `M.fmod` j
    in if k < 0 then fmod (i + j) j else k

instance BinaryOp Double where
    fold2 a b = fold_ a (-b) b
    modE = fmod
    roundUp a b = if b == 0 then a else fceiling (a/b + 0.5) * b
    wrap2 a b = wrap_ a (-b) b

instance BinaryOp UGen where
    iDiv = mkBinaryOperator IDiv iDiv
    modE = mkBinaryOperator Mod fmod
    bitAnd = mkBinaryOperator BitAnd bitAnd
    bitOr = mkBinaryOperator BitOr bitOr
    bitXOr = mkBinaryOperator BitXor bitXOr
    lcmE = mkBinaryOperator LCM lcmE
    gcdE = mkBinaryOperator GCD gcdE
    roundUp = mkBinaryOperator RoundUp roundUp
    trunc = mkBinaryOperator Trunc trunc
    atan2E = mkBinaryOperator Atan2 atan2E
    hypot = mkBinaryOperator Hypot hypot
    hypotx = mkBinaryOperator Hypotx hypotx
    shiftLeft = mkBinaryOperator ShiftLeft shiftLeft
    shiftRight = mkBinaryOperator ShiftRight shiftRight
    unsignedShift = mkBinaryOperator UnsignedShift unsignedShift
    fill = mkBinaryOperator Fill fill
    ring1 = mkBinaryOperator Ring1 ring1
    ring2 = mkBinaryOperator Ring2 ring2
    ring3 = mkBinaryOperator Ring3 ring3
    ring4 = mkBinaryOperator Ring4 ring4
    difSqr = mkBinaryOperator DifSqr difSqr
    sumSqr = mkBinaryOperator SumSqr sumSqr
    sqrSum = mkBinaryOperator SqrSum sqrSum
    sqrDif = mkBinaryOperator SqrDif sqrDif
    absDif = mkBinaryOperator AbsDif absDif
    thresh = mkBinaryOperator Thresh thresh
    amClip = mkBinaryOperator AMClip amClip
    scaleNeg = mkBinaryOperator ScaleNeg scaleNeg
    clip2 = mkBinaryOperator Clip2 clip2
    excess = mkBinaryOperator Excess excess
    fold2 = mkBinaryOperator Fold2 fold2
    wrap2 = mkBinaryOperator Wrap2 wrap2
    firstArg = mkBinaryOperator FirstArg firstArg
    randRange = mkBinaryOperator RandRange randRange
    exprandRange = mkBinaryOperator ExpRandRange exprandRange

wrap_ :: Double -> Double -> Double -> Double
wrap_ a b c =
    let r = c - b
    in if a >= b && a <= c then a else a - r * ffloor (a-b)/r

-- | Fold to within range (i,j).
fold' :: (Ord a,Num a) => a -> a -> a -> a
fold' i j =
    let f n = if n > j
              then f (j - (n - j))
              else if n < i
                   then f (i - (n - i))
                   else n
    in f

-- | Variant with SC3 argument ordering.
fold_ :: (Ord a,Num a) => a -> a -> a -> a
fold_ n i j = fold' i j n

-- | Clip to within range (i,j),
clip' :: (Ord a) => a -> a -> a -> a
clip' i j n = if n < i then i else if n > j then j else n

-- | Variant with SC3 argument ordering.
clip_ :: (Ord a) => a -> a -> a -> a
clip_ n i j = clip' i j n
