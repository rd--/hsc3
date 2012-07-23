-- | Non-standard mathematical classes and class instances.
module Sound.SC3.UGen.Math where

import qualified Data.Fixed as F
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
    a <* b = if a < b then 1.0 else 0.0
    a <=* b = if a <= b then 1.0 else 0.0
    a >* b = if a > b then 1.0 else 0.0
    a >=* b = if a >= b then 1.0 else 0.0

instance OrdE UGen where
    (<*) = mkBinaryOperator LT_ (<*)
    (<=*) = mkBinaryOperator LE (<=*)
    (>*) = mkBinaryOperator GT_ (>*)
    (>=*) = mkBinaryOperator GE (>=*)

-- | Variant of 'RealFrac' with non 'Integral' results.
class RealFracE a where
  properFractionE :: a -> (a,a)
  truncateE :: a -> a
  roundE :: a -> a
  ceilingE :: a -> a
  floorE :: a -> a

-- | Variant of 'truncate'.
truncatef :: RealFrac a => a -> a
truncatef a = fromIntegral (truncate a :: Integer)

-- | Variant of 'round'.
roundf :: RealFrac a => a -> a
roundf a = fromIntegral (round a :: Integer)

-- | Variant of 'ceiling'.
ceilingf :: RealFrac a => a -> a
ceilingf a = fromIntegral (ceiling a :: Integer)

-- | Variant of 'floor'.
floorf :: RealFrac a => a -> a
floorf a = fromIntegral (floor a :: Integer)

instance RealFracE Double where
    properFractionE n =
        let (i,j) = properFraction n
        in (fromIntegral (i::Integer),j)
    truncateE = truncatef
    roundE = roundf
    ceilingE = ceilingf
    floorE = floorf

-- | Variant of @SC3@ @roundTo@ function.
roundTo_ :: Double -> Double -> Double
roundTo_ a b = if b == 0 then a else floorf (a/b + 0.5) * b

-- | 'UGen' form or 'roundTo_'.
roundTo :: UGen -> UGen -> UGen
roundTo = mkBinaryOperator Round roundTo_

instance RealFracE UGen where
    properFractionE = error "RealFracE,UGen,partial"
    truncateE = error "RealFracE,UGen,partial"
    roundE i = roundTo i 1
    ceilingE = mkUnaryOperator Ceil ceilingf
    floorE = mkUnaryOperator Floor floorf

-- | 'UGen' form of 'ceilingE'.
ceil :: UGen -> UGen
ceil = ceilingE

-- | 'Floating' form of 'midiCPS'.
midiCPS' :: Floating a => a -> a
midiCPS' i = 440.0 * (2.0 ** ((i - 69.0) * (1.0 / 12.0)))

-- | Unary operator class.
--
-- > map (floor . (* 1e4) . dbAmp) [-90,-60,-30,0] == [0,10,316,10000]
class (Floating a, Ord a) => UnaryOp a where
    ampDb :: a -> a
    ampDb a = log10 a * 20
    asFloat :: a -> a
    asFloat = error "asFloat"
    asInt :: a -> a
    asInt = error "asInt"
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
    wrap2 :: a -> a -> a

-- | The SC3 @%@ operator is libc fmod function.
--
-- > 1.5 % 1.2 // ~= 0.3
-- > -1.5 % 1.2 // ~= 0.9
-- > 1.5 % -1.2 // ~= -0.9
-- > -1.5 % -1.2 // ~= -0.3
--
-- > 1.5 `fmod` 1.2 -- ~= 0.3
-- > (-1.5) `fmod` 1.2 -- ~= 0.9
-- > 1.5 `fmod` (-1.2) -- ~= -0.9
-- > (-1.5) `fmod` (-1.2) -- ~= -0.3
--
-- 1.2 % 1.5 // ~= 1.2
-- -1.2 % 1.5 // ~= 0.3
-- 1.2 % -1.5 // ~= -0.3
-- -1.2 % -1.5 // ~= -1.2
--
-- > 1.2 `fmod` 1.5 -- ~= 1.2
-- > (-1.2) `fmod` 1.5 -- ~= 0.3
-- > 1.2 `fmod` (-1.5) -- ~= -0.3
-- > (-1.2) `fmod` (-1.5) -- ~= -1.2
fmod :: Double -> Double -> Double
fmod = F.mod'

instance BinaryOp Double where
    fold2 a b = fold_ a (-b) b
    modE = fmod
    roundUp a b = if b == 0 then a else ceilingf (a/b + 0.5) * b
    wrap2 a b = wrap_ a (-b) b

instance BinaryOp UGen where
    iDiv = mkBinaryOperator IDiv iDiv
    modE = mkBinaryOperator Mod fmod
    lcmE = mkBinaryOperator LCM lcmE
    gcdE = mkBinaryOperator GCD gcdE
    roundUp = mkBinaryOperator RoundUp roundUp
    trunc = mkBinaryOperator Trunc trunc
    atan2E = mkBinaryOperator Atan2 atan2E
    hypot = mkBinaryOperator Hypot hypot
    hypotx = mkBinaryOperator Hypotx hypotx
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

-- | Wrap /k/ to within range /(i,j)/, ie. @AbstractFunction.wrap@.
--
-- > map (wrap' 5 10) [3..12] == [8,9,5,6,7,8,9,10,6,7]
wrap' :: Double -> Double -> Double -> Double
wrap' i j k =
    let r = j - i
    in if k >= i && k <= j
       then k
       else k - r * floorf ((k-i) / r)

-- | Generic variant of 'wrap''.
--
-- > map (genericWrap (5::Integer) 10) [3..12] == [8,9,5,6,7,8,9,10,6,7]
genericWrap :: (Ord a, Num a) => a -> a -> a -> a
genericWrap l r n =
    let d = r - l
        f = genericWrap l r
    in if n < l
       then f (n + d)
       else if n > r then f (n - d) else n

-- | Variant of 'wrap'' with @SC3@ argument ordering.
--
-- > map (\n -> wrap_ n 5 10) [3..12] == map (wrap' 5 10) [3..12]
wrap_ :: Double -> Double -> Double -> Double
wrap_ a b c = wrap' b c a

-- | Fold /k/ to within range /(i,j)/, ie. @AbstractFunction.fold@
--
-- > map (foldToRange 5 10) [3..12] == [7,6,5,6,7,8,9,10,9,8]
foldToRange :: (Ord a,Num a) => a -> a -> a -> a
foldToRange i j =
    let f n = if n > j
              then f (j - (n - j))
              else if n < i
                   then f (i - (n - i))
                   else n
    in f

-- | Variant of 'foldToRange' with @SC3@ argument ordering.
fold_ :: (Ord a,Num a) => a -> a -> a -> a
fold_ n i j = foldToRange i j n

-- | Clip /k/ to within range /(i,j)/,
--
-- > map (clip' 5 10) [3..12] == [5,5,5,6,7,8,9,10,10,10]
clip' :: (Ord a) => a -> a -> a -> a
clip' i j n = if n < i then i else if n > j then j else n

-- | Variant of 'clip'' with @SC3@ argument ordering.
clip_ :: (Ord a) => a -> a -> a -> a
clip_ n i j = clip' i j n
