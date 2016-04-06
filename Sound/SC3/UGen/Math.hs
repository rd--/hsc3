-- | Non-standard mathematical classes and class instances.
module Sound.SC3.UGen.Math where

import qualified Data.Fixed as F {- base -}
import Data.Int {- base -}

import Sound.SC3.Common.Math
import Sound.SC3.UGen.Bindings.DB (mulAdd)
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Type

-- | Pseudo-infinite constant UGen.
dinf :: UGen
dinf = constant (9e8::Float)

-- | True is conventionally 1.  The test to determine true is @> 0@.
sc3_true :: Num n => n
sc3_true = 1

-- | False is conventionally 0.
sc3_false :: Num n => n
sc3_false = 0

-- | Lifted 'not'.
--
-- > sc3_not sc3_true == sc3_false
-- > sc3_not sc3_false == sc3_true
sc3_not :: (Ord n,Num n) => n -> n
sc3_not = sc3_bool . not . (> 0)

-- | Translate 'Bool' to 'sc3_true' and 'sc3_false'.
sc3_bool :: Num n => Bool -> n
sc3_bool b = if b then sc3_true else sc3_false

-- | Lift comparison function.
sc3_comparison :: Num n => (n -> n -> Bool) -> n -> n -> n
sc3_comparison f p q = sc3_bool (f p q)

-- | Lifted '=='.
sc3_eq :: (Num n, Eq n) => n -> n -> n
sc3_eq = sc3_comparison (==)

-- | Lifted '/='.
sc3_neq :: (Num n, Eq n) => n -> n -> n
sc3_neq = sc3_comparison (/=)

-- | Lifted '<'.
sc3_lt :: (Num n, Ord n) => n -> n -> n
sc3_lt = sc3_comparison (<)

-- | Lifted '<='.
sc3_lte :: (Num n, Ord n) => n -> n -> n
sc3_lte = sc3_comparison (<=)

-- | Lifted '>'.
sc3_gt :: (Num n, Ord n) => n -> n -> n
sc3_gt = sc3_comparison (>)

-- | Lifted '>='.
sc3_gte :: (Num n, Ord n) => n -> n -> n
sc3_gte = sc3_comparison (>=)

-- | Variant of @SC3@ @roundTo@ function.
--
-- > let r = [0,0,0.25,0.25,0.5,0.5,0.5,0.75,0.75,1,1]
-- > in map (`roundTo_` 0.25) [0,0.1 .. 1] == r
roundTo_ :: (RealFrac n, Ord n) => n -> n -> n
roundTo_ = sc3_round_to

-- > map (flip sc3_round_to 0.25) [0,0.1 .. 1]
sc3_round_to :: (RealFrac n, Ord n) => n -> n -> n
sc3_round_to a b = if b == 0 then a else sc3_floor ((a / b) + 0.5) * b

sc3_idiv :: RealFrac n => n -> n -> n
sc3_idiv a b = fromInteger (floor a `div` floor b)

-- | The SC3 @%@ operator is the 'F.mod'' function.
--
-- > > 1.5 % 1.2 // ~= 0.3
-- > > -1.5 % 1.2 // ~= 0.9
-- > > 1.5 % -1.2 // ~= -0.9
-- > > -1.5 % -1.2 // ~= -0.3
--
-- > let (%) = sc3_mod
-- > 1.5 % 1.2 -- ~= 0.3
-- > (-1.5) % 1.2 -- ~= 0.9
-- > 1.5 % (-1.2) -- ~= -0.9
-- > (-1.5) % (-1.2) -- ~= -0.3
--
-- > > 1.2 % 1.5 // ~= 1.2
-- > > -1.2 % 1.5 // ~= 0.3
-- > > 1.2 % -1.5 // ~= -0.3
-- > > -1.2 % -1.5 // ~= -1.2
--
-- > 1.2 % 1.5 -- ~= 1.2
-- > (-1.2) % 1.5 -- ~= 0.3
-- > 1.2 % (-1.5) -- ~= -0.3
-- > (-1.2) % (-1.5) -- ~= -1.2
sc3_mod :: RealFrac n => n -> n -> n
sc3_mod = F.mod'

-- | Association table for 'Binary' to haskell function implementing operator.
binop_hs_tbl :: (Real n,Floating n,RealFrac n,Ord n) => [(Binary,n -> n -> n)]
binop_hs_tbl =
    [(Add,(+))
    ,(Sub,(-))
    ,(FDiv,(/))
    ,(IDiv,sc3_idiv)
    ,(Mod,sc3_mod)
    ,(EQ_,sc3_eq)
    ,(NE,sc3_neq)
    ,(LT_,sc3_lt)
    ,(LE,sc3_lte)
    ,(GT_,sc3_gt)
    ,(GE,sc3_gte)
    ,(Min,min)
    ,(Max,max)
    ,(Mul,(*))
    ,(Pow,(**))
    ,(Min,min)
    ,(Max,max)
    ,(Round,sc3_round_to)]

-- | 'lookup' 'binop_hs_tbl' via 'toEnum'.
binop_special_hs :: (Real n,RealFrac n,Floating n, Ord n) => Int -> Maybe (n -> n -> n)
binop_special_hs z = lookup (toEnum z) binop_hs_tbl

-- | Association table for 'Unary' to haskell function implementing operator.
uop_hs_tbl :: (RealFrac n,Floating n,Ord n) => [(Unary,n -> n)]
uop_hs_tbl =
    [(Neg,negate)
    ,(Not,\z -> if z > 0 then 0 else 1)
    ,(Abs,abs)
    ,(Ceil,sc3_ceiling)
    ,(Floor,sc3_floor)
    ,(Squared,\z -> z * z)
    ,(Cubed,\z -> z * z * z)
    ,(Sqrt,sqrt)
    ,(Recip,recip)
    ,(MIDICPS,midi_to_cps)
    ,(CPSMIDI,cps_to_midi)
    ,(Sin,sin)
    ,(Cos,cos)
    ,(Tan,tan)]

-- | 'lookup' 'uop_hs_tbl' via 'toEnum'.
uop_special_hs :: (RealFrac n,Floating n, Ord n) => Int -> Maybe (n -> n)
uop_special_hs z = lookup (toEnum z) uop_hs_tbl

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

-- | Variant on Eq class, result is of the same type as the values compared.
class (Eq a,Num a) => EqE a where
    (==*) :: a -> a -> a
    (==*) = sc3_eq
    (/=*) :: a -> a -> a
    (/=*) = sc3_neq

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
    (<*) = sc3_lt
    (<=*) :: a -> a -> a
    (<=*) = sc3_lte
    (>*) :: a -> a -> a
    (>*) = sc3_gt
    (>=*) :: a -> a -> a
    (>=*) = sc3_gte

instance OrdE Int
instance OrdE Integer
instance OrdE Int32 where
instance OrdE Int64 where
instance OrdE Float
instance OrdE Double

instance OrdE UGen where
    (<*) = mkBinaryOperator LT_ sc3_lt
    (<=*) = mkBinaryOperator LE sc3_lte
    (>*) = mkBinaryOperator GT_ sc3_gt
    (>=*) = mkBinaryOperator GE sc3_gte

sc3_properFraction :: (RealFrac t, Num t) => t -> (t,t)
sc3_properFraction a =
    let (p,q) = properFraction a
    in (fromInteger p,q)

sc3_truncate :: (RealFrac a, Num a) => a -> a
sc3_truncate = fromInteger . truncate

sc3_round :: (RealFrac a, Num a) => a -> a
sc3_round = fromInteger . round

sc3_ceiling :: (RealFrac a, Num a) => a -> a
sc3_ceiling = fromInteger . ceiling

sc3_floor :: (RealFrac a, Num a) => a -> a
sc3_floor = fromInteger . floor

-- | Variant of 'RealFrac' with non 'Integral' results.
class RealFrac a => RealFracE a where
  properFractionE :: a -> (a,a)
  properFractionE = sc3_properFraction
  truncateE :: a -> a
  truncateE = sc3_truncate
  roundE :: a -> a
  roundE = sc3_round
  ceilingE :: a -> a
  ceilingE = sc3_ceiling
  floorE :: a -> a
  floorE = sc3_floor

instance RealFracE Float
instance RealFracE Double

-- | 'UGen' form or 'roundTo_'.
roundTo :: UGen -> UGen -> UGen
roundTo = mkBinaryOperator Round roundTo_

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
    ampDb = amp_to_db
    asFloat :: a -> a
    asFloat = error "asFloat"
    asInt :: a -> a
    asInt = error "asInt"
    cpsMIDI :: a -> a
    cpsMIDI = cps_to_midi
    cpsOct :: a -> a
    cpsOct = cps_to_oct
    cubed :: a -> a
    cubed n = n * n * n
    dbAmp :: a -> a
    dbAmp = db_to_amp
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
    midiCPS = midi_to_cps
    midiRatio :: a -> a
    midiRatio = midi_to_ratio
    notE :: a -> a
    notE a = if a > 0.0 then 0.0 else 1.0
    notNil :: a -> a
    notNil a = if a /= 0.0 then 0.0 else 1.0
    octCPS :: a -> a
    octCPS = oct_to_cps
    ramp_ :: a -> a
    ramp_ _ = error "ramp_"
    ratioMIDI :: a -> a
    ratioMIDI = ratio_to_midi
    softClip :: a -> a
    softClip = error "softClip"
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

difSqr' :: Num a => a -> a -> a
difSqr' a b = (a * a) - (b * b)

hypotx' :: (Ord a, Floating a) => a -> a -> a
hypotx' x y = abs x + abs y - ((sqrt 2 - 1) * min (abs x) (abs y))

-- | Binary operator class.
class (Floating a,RealFrac a, Ord a) => BinaryOp a where
    absDif :: a -> a -> a
    absDif a b = abs (a - b)
    amClip :: a -> a -> a
    amClip a b = if b <= 0 then 0 else a * b
    atan2E :: a -> a -> a
    atan2E a b = atan (b/a)
    clip2 :: a -> a -> a
    clip2 a b = clip_ a (-b) b
    difSqr :: a -> a -> a
    difSqr = difSqr'
    excess :: a -> a -> a
    excess a b = a - clip_ a (-b) b
    exprandRange :: a -> a -> a
    exprandRange = error "exprandRange"
    fill :: a -> a -> a
    fill = error "fill"
    firstArg :: a -> a -> a
    firstArg a _ = a
    fold2 :: a -> a -> a
    fold2 a b = fold_ a (-b) b
    gcdE :: a -> a -> a
    gcdE = error "gcdE"
    hypot :: a -> a -> a
    hypot x y = sqrt (x * x + y * y)
    hypotx :: a -> a -> a
    hypotx = hypotx'
    iDiv :: a -> a -> a
    iDiv = sc3_idiv
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

fmod_f32 :: Float -> Float -> Float
fmod_f32 = sc3_mod

fmod_f64 :: Double -> Double -> Double
fmod_f64 = sc3_mod

instance BinaryOp Float where
    fold2 a b = fold_ a (-b) b
    modE = F.mod'
    roundUp a b = if b == 0 then a else ceilingE (a/b + 0.5) * b
    wrap2 a b = wrap_ a (-b) b

instance BinaryOp Double where
    fold2 a b = fold_ a (-b) b
    modE = F.mod'
    roundUp a b = if b == 0 then a else ceilingE (a/b + 0.5) * b
    wrap2 a b = wrap_ a (-b) b

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

-- | Wrap /k/ to within range /(i,j)/, ie. @AbstractFunction.wrap@.
--
-- > > [5,6].wrap(0,5) == [5,0]
-- > map (wrap' 0 5) [5,6] == [5,0]
--
-- > > [9,10,5,6,7,8,9,10,5,6].wrap(5,10) == [9,10,5,6,7,8,9,10,5,6]
-- > map (wrap' 5 10) [3..12] == [9,10,5,6,7,8,9,10,5,6]
wrap' :: RealFracE n => n -> n -> n -> n
wrap' i j k =
    let r = j - i + 1
    in if k >= i && k <= j
       then k
       else k - r * floorE ((k-i) / r)

-- | Generic variant of 'wrap''.
--
-- > > [5,6].wrap(0,5) == [5,0]
-- > map (genericWrap 0 5) [5,6] == [5,0]
--
-- > > [9,10,5,6,7,8,9,10,5,6].wrap(5,10) == [9,10,5,6,7,8,9,10,5,6]
-- > map (genericWrap (5::Integer) 10) [3..12] == [9,10,5,6,7,8,9,10,5,6]
genericWrap :: (Ord a, Num a) => a -> a -> a -> a
genericWrap l r n =
    let d = r - l + 1
        f = genericWrap l r
    in if n < l
       then f (n + d)
       else if n > r then f (n - d) else n

-- | Variant of 'wrap'' with @SC3@ argument ordering.
--
-- > map (\n -> wrap_ n 5 10) [3..12] == map (wrap' 5 10) [3..12]
wrap_ :: RealFracE n => n -> n -> n -> n
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

hypot_ :: (Floating a) => a -> a -> a
hypot_ x y = sqrt (x * x + y * y)

-- | Map from one linear range to another linear range.
linlin_ma :: (Fractional a,MulAdd a) => a -> a -> a -> a -> a -> a
linlin_ma i sl sr dl dr = let (m,a) = linlin_muladd sl sr dl dr in mul_add i m a

-- | Scale uni-polar (0,1) input to linear (l,r) range
urange_ma :: (Fractional a,MulAdd a) => a -> a -> a -> a
urange_ma l r i = let m = r - l in mul_add i m l

-- | Scale bi-polar (-1,1) input to linear (l,r) range.  Note that the
-- argument order is not the same as 'linLin'.
range_ma :: (Fractional a,MulAdd a) => a -> a -> a -> a
range_ma l r i = let (m,a) = range_muladd l r in mul_add i m a
