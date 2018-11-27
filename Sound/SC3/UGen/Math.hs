-- | Non-standard mathematical classes and class instances.
module Sound.SC3.UGen.Math where

import qualified Data.Fixed as F {- base -}
import Data.Int {- base -}

import qualified Sound.SC3.Common.Math as Math
import Sound.SC3.UGen.Bindings.DB (mulAdd)
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Type

-- | Pseudo-infinite constant UGen.
dinf :: UGen
dinf = constant (9e8::Float)

-- | Association table for 'Binary' to haskell function implementing operator.
binop_hs_tbl :: (Real n,Floating n,RealFrac n) => [(Binary,n -> n -> n)]
binop_hs_tbl =
    [(Add,(+))
    ,(Sub,(-))
    ,(FDiv,(/))
    ,(IDiv,Math.sc3_idiv)
    ,(Mod,Math.sc3_mod)
    ,(EQ_,Math.sc3_eq)
    ,(NE,Math.sc3_neq)
    ,(LT_,Math.sc3_lt)
    ,(LE,Math.sc3_lte)
    ,(GT_,Math.sc3_gt)
    ,(GE,Math.sc3_gte)
    ,(Min,min)
    ,(Max,max)
    ,(Mul,(*))
    ,(Pow,(**))
    ,(Min,min)
    ,(Max,max)
    ,(Round,Math.sc3_round_to)]

-- | 'lookup' 'binop_hs_tbl' via 'toEnum'.
binop_special_hs :: (RealFrac n,Floating n) => Int -> Maybe (n -> n -> n)
binop_special_hs z = lookup (toEnum z) binop_hs_tbl

-- | Association table for 'Unary' to haskell function implementing operator.
uop_hs_tbl :: (RealFrac n,Floating n) => [(Unary,n -> n)]
uop_hs_tbl =
    [(Neg,negate)
    ,(Not,\z -> if z > 0 then 0 else 1)
    ,(Abs,abs)
    ,(Ceil,Math.sc3_ceiling)
    ,(Floor,Math.sc3_floor)
    ,(Squared,\z -> z * z)
    ,(Cubed,\z -> z * z * z)
    ,(Sqrt,sqrt)
    ,(Recip,recip)
    ,(MIDICPS,Math.midi_to_cps)
    ,(CPSMIDI,Math.cps_to_midi)
    ,(Sin,sin)
    ,(Cos,cos)
    ,(Tan,tan)]

-- | 'lookup' 'uop_hs_tbl' via 'toEnum'.
uop_special_hs :: (RealFrac n,Floating n) => Int -> Maybe (n -> n)
uop_special_hs z = lookup (toEnum z) uop_hs_tbl

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

-- | Variant on Eq class, result is of the same type as the values compared.
class (Eq a,Num a) => EqE a where
    (==*) :: a -> a -> a
    (==*) = Math.sc3_eq
    (/=*) :: a -> a -> a
    (/=*) = Math.sc3_neq

instance EqE Int where
instance EqE Integer where
instance EqE Int32 where
instance EqE Int64 where
instance EqE Float where
instance EqE Double where

instance EqE UGen where
    (==*) = mkBinaryOperator EQ_ (==*)
    (/=*) = mkBinaryOperator NE (/=*)

-- | Variant on Ord class, result is of the same type as the values compared.
class (Ord a,Num a) => OrdE a where
    (<*) :: a -> a -> a
    (<*) = Math.sc3_lt
    (<=*) :: a -> a -> a
    (<=*) = Math.sc3_lte
    (>*) :: a -> a -> a
    (>*) = Math.sc3_gt
    (>=*) :: a -> a -> a
    (>=*) = Math.sc3_gte

instance OrdE Int
instance OrdE Integer
instance OrdE Int32
instance OrdE Int64
instance OrdE Float
instance OrdE Double

instance OrdE UGen where
    (<*) = mkBinaryOperator LT_ Math.sc3_lt
    (<=*) = mkBinaryOperator LE Math.sc3_lte
    (>*) = mkBinaryOperator GT_ Math.sc3_gt
    (>=*) = mkBinaryOperator GE Math.sc3_gte

-- | Variant of 'RealFrac' with non 'Integral' results.
class RealFrac a => RealFracE a where
  properFractionE :: a -> (a,a)
  properFractionE = Math.sc3_properFraction
  truncateE :: a -> a
  truncateE = Math.sc3_truncate
  roundE :: a -> a
  roundE = Math.sc3_round
  ceilingE :: a -> a
  ceilingE = Math.sc3_ceiling
  floorE :: a -> a
  floorE = Math.sc3_floor

instance RealFracE Float
instance RealFracE Double

-- | 'UGen' form or 'Math.sc3_round_to'.
roundTo :: UGen -> UGen -> UGen
roundTo = mkBinaryOperator Round Math.sc3_round_to

instance RealFracE UGen where
    properFractionE = error "UGen.properFractionE"
    truncateE = error "UGen.truncateE"
    roundE i = roundTo i 1
    ceilingE = mkUnaryOperator Ceil ceilingE
    floorE = mkUnaryOperator Floor floorE

-- | 'UGen' form of 'ceilingE'.
ceil :: UGen -> UGen
ceil = ceilingE

-- | Unary operator class.
--
-- > map (floor . (* 1e4) . dbAmp) [-90,-60,-30,0] == [0,10,316,10000]
class (Floating a, Ord a) => UnaryOp a where
    ampDb :: a -> a
    ampDb = Math.amp_to_db
    asFloat :: a -> a
    asFloat = error "asFloat"
    asInt :: a -> a
    asInt = error "asInt"
    cpsMIDI :: a -> a
    cpsMIDI = Math.cps_to_midi
    cpsOct :: a -> a
    cpsOct = Math.cps_to_oct
    cubed :: a -> a
    cubed n = n * n * n
    dbAmp :: a -> a
    dbAmp = Math.db_to_amp
    distort :: a -> a
    distort = Math.sc3_distort
    frac :: a -> a
    frac = error "frac"
    isNil :: a -> a
    isNil a = if a == 0.0 then 0.0 else 1.0
    log10 :: a -> a
    log10 = logBase 10
    log2 :: a -> a
    log2 = logBase 2
    midiCPS :: a -> a
    midiCPS = Math.midi_to_cps
    midiRatio :: a -> a
    midiRatio = Math.midi_to_ratio
    notE :: a -> a
    notE a = if a > 0.0 then 0.0 else 1.0
    notNil :: a -> a
    notNil a = if a /= 0.0 then 0.0 else 1.0
    octCPS :: a -> a
    octCPS = Math.oct_to_cps
    ramp_ :: a -> a
    ramp_ _ = error "ramp_"
    ratioMIDI :: a -> a
    ratioMIDI = Math.ratio_to_midi
    softClip :: a -> a
    softClip = Math.sc3_softclip
    squared :: a -> a
    squared = \z -> z * z

instance UnaryOp Float where
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
    ramp_ = mkUnaryOperator Ramp_ ramp_
    ratioMIDI = mkUnaryOperator RatioMIDI ratioMIDI
    softClip = mkUnaryOperator SoftClip softClip
    squared = mkUnaryOperator Squared squared

-- | Binary operator class.
class (Floating a,RealFrac a, Ord a) => BinaryOp a where
    absDif :: a -> a -> a
    absDif a b = abs (a - b)
    amClip :: a -> a -> a
    amClip a b = if b <= 0 then 0 else a * b
    atan2E :: a -> a -> a
    atan2E a b = atan (b/a)
    clip2 :: a -> a -> a
    clip2 a b = Math.sc3_clip a (-b) b
    difSqr :: a -> a -> a
    difSqr = Math.sc3_dif_sqr
    excess :: a -> a -> a
    excess a b = a - Math.sc3_clip a (-b) b
    exprandRange :: a -> a -> a
    exprandRange = error "exprandRange"
    fill :: a -> a -> a
    fill = error "fill"
    firstArg :: a -> a -> a
    firstArg a _ = a
    fold2 :: a -> a -> a
    fold2 a b = Math.sc3_fold a (-b) b
    gcdE :: a -> a -> a
    gcdE = error "gcdE"
    hypot :: a -> a -> a
    hypot = Math.sc3_hypot
    hypotx :: a -> a -> a
    hypotx = Math.sc3_hypotx
    iDiv :: a -> a -> a
    iDiv = Math.sc3_idiv
    lcmE :: a -> a -> a
    lcmE = error "lcmE"
    modE :: a -> a -> a
    modE = error "modE"
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
    roundUp = error "roundUp"
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
    wrap2 = error "wrap2"

instance BinaryOp Float where
    fold2 a b = Math.sc3_fold a (-b) b
    modE = F.mod'
    roundUp a b = if b == 0 then a else ceilingE (a/b + 0.5) * b
    wrap2 a b = Math.sc3_wrap_ni a (-b) b

instance BinaryOp Double where
    fold2 a b = Math.sc3_fold a (-b) b
    modE = F.mod'
    roundUp a b = if b == 0 then a else ceilingE (a/b + 0.5) * b
    wrap2 a b = Math.sc3_wrap_ni a (-b) b

instance BinaryOp UGen where
    iDiv = mkBinaryOperator IDiv iDiv
    modE = mkBinaryOperator Mod F.mod'
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

-- | MulAdd operator class.
class Num a => MulAdd a where
    mul_add :: a -> a -> a -> a
    mul_add i m a = i * m + a

instance MulAdd UGen where mul_add = mulAdd
instance MulAdd Float where
instance MulAdd Double where

-- | Map from one linear range to another linear range.
linlin_ma :: (Fractional a,MulAdd a) => a -> a -> a -> a -> a -> a
linlin_ma i sl sr dl dr = let (m,a) = Math.linlin_muladd sl sr dl dr in mul_add i m a

-- | Scale uni-polar (0,1) input to linear (l,r) range
urange_ma :: (Fractional a,MulAdd a) => a -> a -> a -> a
urange_ma l r i = let m = r - l in mul_add i m l

-- | Scale bi-polar (-1,1) input to linear (l,r) range.  Note that the
-- argument order is not the same as 'linLin'.
range_ma :: (Fractional a,MulAdd a) => a -> a -> a -> a
range_ma l r i = let (m,a) = Math.range_muladd l r in mul_add i m a
