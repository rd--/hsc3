-- | Common math functions.
module Sound.Sc3.Common.Math where

import qualified Data.Fixed {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import qualified Numeric {- base -}

import qualified Sound.Sc3.Common.Base as Common.Base {- hsc3 -}

{- | Half pi.

>>> half_pi
1.5707963267948966
-}
half_pi :: Floating a => a
half_pi = pi / 2

{- | Two pi.

>>> two_pi
6.283185307179586
-}
two_pi :: Floating n => n
two_pi = 2 * pi

-- | 'abs' of '(-)'.
absdif :: Num a => a -> a -> a
absdif i j = abs (j - i)

-- | Sc3 MulAdd type signature, arguments in Sc3 order of input, multiply, add.
type Sc3_MulAdd t = t -> t -> t -> t

{- | Ordinary (un-optimised) multiply-add, see also mulAdd Ugen.

>>> sc3_mul_add 2 3 4 == 2 * 3 + 4
True

>>> map (\x -> sc3_mul_add x 2 3) [1,5]
[5,13]

>>> map (\x -> sc3_mul_add x 3 2) [1,5]
[5,17]
-}
sc3_mul_add :: Num t => Sc3_MulAdd t
sc3_mul_add i m a = i * m + a

{- | Ordinary Haskell order (un-optimised) multiply-add.

>>> mul_add 3 4 2 == 2 * 3 + 4
True

>>> map (mul_add 2 3) [1,5]
[5,13]

>> map (mul_add 3 4) [1,5]
[7,19]
-}
mul_add :: Num t => t -> t -> t -> t
mul_add m a = (+ a) . (* m)

{- | 'uncurry' 'mul_add'

>>> mul_add_hs (3,4) 2 == 2 * 3 + 4
True
-}
mul_add_hs :: Num t => (t, t) -> t -> t
mul_add_hs = uncurry mul_add

-- | 'fromInteger' of 'truncate'.
sc3_truncate :: RealFrac a => a -> a
sc3_truncate = fromInteger . truncate

-- | 'fromInteger' of 'round'.
sc3_round :: RealFrac a => a -> a
sc3_round = fromInteger . round

-- | 'fromInteger' of 'ceiling'.
sc3_ceiling :: RealFrac a => a -> a
sc3_ceiling = fromInteger . ceiling

-- | 'fromInteger' of 'floor'.
sc3_floor :: RealFrac a => a -> a
sc3_floor = fromInteger . floor

{- | Variant of @Sc3@ @roundTo@ function.

>>> sc3_round_to (2/3) 0.25
0.75

>>> map (`sc3_round_to` 0.25) [0,0.1 .. 1]
[0.0,0.0,0.25,0.25,0.5,0.5,0.5,0.75,0.75,1.0,1.0]

>>> map (`sc3_round_to` 5.0) [100.0 .. 110.0]
[100.0,100.0,100.0,105.0,105.0,105.0,105.0,105.0,110.0,110.0,110.0]
-}
sc3_round_to :: RealFrac n => n -> n -> n
sc3_round_to a b = if b == 0 then a else sc3_floor ((a / b) + 0.5) * b

-- | 'fromInteger' of 'div' of 'floor'.
sc3_idiv :: RealFrac n => n -> n -> n
sc3_idiv a b = fromInteger (floor a `div` floor b)

{- | 'sc3_lcm'

Least common multiple. This definition extends the usual definition
and returns a negative number if any of the operands is negative. This
makes it consistent with the lattice-theoretical interpretation and
its idempotency, commutative, associative, absorption laws.

>>> lcm 4 6
12

>>> lcm 1 1
1

>>> lcm 1624 26
21112

>>> lcm 1624 (-26) /= (-21112)
True

>>> lcm (-1624) (-26) /= (-21112)
True

>>> lcm 513 (gcd 513 44)
513
-}
sc3_lcm :: t -> t -> t
sc3_lcm = error "sc3_lcm: undefined"

{- | 'sc3_gcd'

Greatest common divisor. This definition extends the usual
definition and returns a negative number if both operands are
negative. This makes it consistent with the lattice-theoretical
interpretation and its idempotency, commutative, associative,
absorption laws. <https://www.jsoftware.com/papers/eem/gcd.htm>

>>> gcd 4 6
2

>>> gcd 0 1
1

>>> gcd 1024 256
256

>>> gcd 1024 (-256)
256

>>> gcd (-1024) (-256) /= (-256)
True

>>> gcd (-1024) (lcm (-1024) 256) /= (-1024)
True

>>> gcd 66 54 * lcm 66 54 == 66 * 54
True
-}
sc3_gcd :: t -> t -> t
sc3_gcd = error "sc3_gcd: undefined"

{- | The Sc3 @%@ Ugen operator is the 'Data.Fixed.mod'' function.

> 1.5 % 1.2 // ~= 0.3
> -1.5 % 1.2 // ~= 0.9
> 1.5 % -1.2 // ~= -0.9
> -1.5 % -1.2 // ~= -0.3

>>> let (~=) p q = abs (p - q) < 0.000001
>>> let (%) = sc3_mod
>>> (1.5 % 1.2) ~= 0.3
True

>>> ((-1.5) % 1.2) ~= 0.9
True

>>> (1.5 % (-1.2)) ~= (-0.9)
True

>>> ((-1.5) % (-1.2)) ~= (-0.3)
True

> 1.2 % 1.5 // ~= 1.2
> -1.2 % 1.5 // ~= 0.3
> 1.2 % -1.5 // ~= -0.3
> -1.2 % -1.5 // ~= -1.2

>>> (1.2 % 1.5) ~= 1.2
True

>>> ((-1.2) % 1.5) ~= 0.3
True

>>> (1.2 % (-1.5)) ~= (-0.3)
True

>>> ((-1.2) % (-1.5)) ~= (-1.2)
True

>>> map (\n -> sc3_mod n 12.0) [-1.0,12.25,15.0]
[11.0,0.25,3.0]
-}
sc3_mod :: RealFrac n => n -> n -> n
sc3_mod = Data.Fixed.mod'

-- | Type specialised 'sc3_mod'.
fmod_f32 :: Float -> Float -> Float
fmod_f32 = sc3_mod

-- | Type specialised 'sc3_mod'.
fmod_f64 :: Double -> Double -> Double
fmod_f64 = sc3_mod

{- | @Sc3@ clip function.  Clip /n/ to within range /(i,j)/.  'clip' is a 'Ugen'.

>>> map (\n -> sc3_clip n 5 10) [3..12]
[5,5,5,6,7,8,9,10,10,10]
-}
sc3_clip :: Ord a => a -> a -> a -> a
sc3_clip n i j = if n < i then i else if n > j then j else n

{- | Variant of 'sc3_clip' with haskell argument structure.

>>> map (clip_hs (5,10)) [3..12]
[5,5,5,6,7,8,9,10,10,10]
-}
clip_hs :: (Ord a) => (a, a) -> a -> a
clip_hs (i, j) n = sc3_clip n i j

{- | Fractional modulo, alternate implementation.

>>> map (\n -> sc3_mod_alt n 12.0) [-1.0,12.25,15.0]
[11.0,0.25,3.0]
-}
sc3_mod_alt :: RealFrac a => a -> a -> a
sc3_mod_alt n hi =
  let lo = 0.0
  in if n >= lo && n < hi
      then n
      else
        if hi == lo
          then lo
          else n - hi * sc3_floor (n / hi)

{- | Wrap function that is /non-inclusive/ at right edge, ie. the Wrap Ugen rule.

>>> map (round . sc3_wrap_ni 0 5) [4,5,6]
[4,0,1]

>>> map (round . sc3_wrap_ni 5 10) [3..12]
[8,9,5,6,7,8,9,5,6,7]

> Sound.Sc3.Plot.plot_fn_r1_ln (sc3_wrap_ni (-1) 1) (-2,2)
-}
sc3_wrap_ni :: RealFrac a => a -> a -> a -> a
sc3_wrap_ni lo hi n = sc3_mod (n - lo) (hi - lo) + lo

{- | sc_wrap::int

> [5,6].wrap(0,5) == [5,0]

>>> map (wrap_hs_int (0,5)) [5,6]
[5,0]

> [9,10,5,6,7,8,9,10,5,6].wrap(5,10) == [9,10,5,6,7,8,9,10,5,6]

>>> map (wrap_hs_int (5,10)) [3..12]
[9,10,5,6,7,8,9,10,5,6]
-}
wrap_hs_int :: Integral a => (a, a) -> a -> a
wrap_hs_int (i, j) n = ((n - i) `mod` (j - i + 1)) + i

{- | Wrap /n/ to within range /(i,j)/, ie. @AbstractFunction.wrap@,
ie. /inclusive/ at right edge.  'wrap' is a 'Ugen', hence prime.

> [5.0,6.0].wrap(0.0,5.0) == [0.0,1.0]

>>> map (round . wrap_hs (0,4)) [-1,0 .. 5]
[3,0,1,2,3,0,1]

>>> map (round . wrap_hs (0,5)) [5,6]
[0,1]

>>> map (round . wrap_hs (5,10)) [3..12]
[8,9,5,6,7,8,9,5,6,7]

> Sound.Sc3.Plot.plot_fn_r1_ln (wrap_hs (-1,1)) (-2,2)
-}
wrap_hs :: RealFrac n => (n, n) -> n -> n
wrap_hs (i, j) n =
  let r = j - i -- + 1
      n' = if n >= j then n - r else if n < i then n + r else n
  in if n' >= i && n' < j
      then n'
      else n' - r * sc3_floor ((n' - i) / r)

{- | Variant of 'wrap_hs' with @Sc3@ argument ordering.

>>> map (\n -> sc3_wrap n 5 10) [3..12] == map (wrap_hs (5,10)) [3..12]
True

>>> map (\n -> sc3_wrap n 0 4) [-1, 0 .. 5]
[3.0,0.0,1.0,2.0,3.0,0.0,1.0]
-}
sc3_wrap :: RealFrac n => n -> n -> n -> n
sc3_wrap index start end = wrap_hs (start, end) index

{- | Generic variant of 'wrap''.

> [5,6].wrap(0,5) == [5,0]

>>> map (generic_wrap (0,5)) [5,6]
[5,0]

> [9,10,5,6,7,8,9,10,5,6].wrap(5,10) == [9,10,5,6,7,8,9,10,5,6]

>>> map (generic_wrap (5::Integer,10)) [3..12]
[9,10,5,6,7,8,9,10,5,6]
-}
generic_wrap :: (Ord a, Num a) => (a, a) -> a -> a
generic_wrap (l, r) n =
  let d = r - l + 1
      f = generic_wrap (l, r)
  in if n < l
      then f (n + d)
      else if n > r then f (n - d) else n

{- | Given sample-rate /sr/ and bin-count /n/ calculate frequency of /i/th bin.

>>> bin_to_freq 44100 2048 32
689.0625
-}
bin_to_freq :: (Fractional n, Integral i) => n -> i -> i -> n
bin_to_freq sr n i = fromIntegral i * sr / fromIntegral n

{- | Fractional midi note number to cycles per second.

>>> map (floor . midi_to_cps) [0,24,69,120,127]
[8,32,440,8372,12543]

>>> map (floor . midi_to_cps) [-36,138]
[1,23679]

>>> map (floor . midi_to_cps) [69.0,69.25 .. 70.0]
[440,446,452,459,466]
-}
midi_to_cps :: Floating a => a -> a
midi_to_cps i = 440.0 * (2.0 ** ((i - 69.0) * (1.0 / 12.0)))

{- | Cycles per second to fractional midi note number.

>>> map (round . cps_to_midi) [8,32,440,8372,12543]
[0,24,69,120,127]

>>> map (round . cps_to_midi) [1,24000]
[-36,138]
-}
cps_to_midi :: Floating a => a -> a
cps_to_midi a = (logBase 2 (a * (1.0 / 440.0)) * 12.0) + 69.0

{- | Cycles per second to linear octave (4.75 = A4 = 440).

>>> map (cps_to_oct . midi_to_cps) [60,63,69]
[4.0,4.25,4.75]
-}
cps_to_oct :: Floating a => a -> a
cps_to_oct a = logBase 2 (a * (1.0 / 440.0)) + 4.75

{- | Linear octave to cycles per second.

> [4.0,4.25,4.75].octcps.cpsmidi == [60,63,69]

>>> map (cps_to_midi . oct_to_cps) [4.0,4.25,4.75]
[60.0,63.0,69.0]
-}
oct_to_cps :: Floating a => a -> a
oct_to_cps a = 440.0 * (2.0 ** (a - 4.75))

-- | Degree, scale and steps per octave to key.
degree_to_key :: RealFrac a => [a] -> a -> a -> a
degree_to_key s n d =
  let l = length s
      d' = round d
      a = (d - fromIntegral d') * 10.0 * (n / 12.0)
  in (n * fromIntegral (d' `div` l)) + Common.Base.at_with_error_message "degree_to_key" s (d' `mod` l) + a

{- | One-indexed piano key number (for standard 88 key piano) to midi note number.

>>> map pianokey_to_midi [1,49,88]
[21,69,108]
-}
pianokey_to_midi :: Num n => n -> n
pianokey_to_midi = (+) 20

{- | Piano key to hertz (ba.pianokey2hz in Faust).
     This is useful as a more musical gamut than midi note numbers.
     Ie. if x is in (0,1) then pianokey_to_cps of (x * 88) is in (26,4168)

>>> map (round . pianokey_to_cps) [0,1,40,49,88]
[26,28,262,440,4186]

>>> map (round . midi_to_cps) [0,60,69,127]
[8,262,440,12544]
-}
pianokey_to_cps :: Floating n => n -> n
pianokey_to_cps = midi_to_cps . pianokey_to_midi

{- | Linear amplitude to decibels.

>>> map (round . amp_to_db) [0.01,0.05,0.0625,0.125,0.25,0.5]
[-40,-26,-24,-18,-12,-6]
-}
amp_to_db :: Floating a => a -> a
amp_to_db = (* 20) . logBase 10

{- | Decibels to linear amplitude.

>>> map (floor . (* 100). db_to_amp) [-40,-26,-24,-18,-12,-6]
[1,5,6,12,25,50]

>>> let amp = map (2 **) [0 .. 15]
>>> let db = [0,-6 .. -90]
>>> map (round . amp_to_db . (/) 1) amp == db
True

>>> db_to_amp (-3)
0.7079457843841379

>>> amp_to_db 0.7079457843841379
-3.0
-}
db_to_amp :: Floating a => a -> a
db_to_amp = (10 **) . (* 0.05)

{- | Fractional midi note interval to frequency multiplier.

>>> map midi_to_ratio [-12,0,7,12]
[0.5,1.0,1.4983070768766815,2.0]
-}
midi_to_ratio :: Floating a => a -> a
midi_to_ratio a = 2.0 ** (a * (1.0 / 12.0))

{- | Inverse of 'midi_to_ratio'.

>>> map ratio_to_midi [3/2,2]
[7.019550008653875,12.0]
-}
ratio_to_midi :: Floating a => a -> a
ratio_to_midi a = 12.0 * logBase 2 a

{- | /sr/ = sample rate, /r/ = cycle (two-pi), /cps/ = frequency

>>> cps_to_incr 48000 128 375
1.0

>>> cps_to_incr 48000 two_pi 458.3662361046586
6.0e-2
-}
cps_to_incr :: Fractional a => a -> a -> a -> a
cps_to_incr sr r cps = (r / sr) * cps

{- | Inverse of 'cps_to_incr'.

>>> incr_to_cps 48000 128 1
375.0
-}
incr_to_cps :: Fractional a => a -> a -> a -> a
incr_to_cps sr r ic = ic / (r / sr)

-- | Pan2 function, identity is linear, sqrt is equal power.
pan2_f :: Fractional t => (t -> t) -> t -> t -> (t, t)
pan2_f f p q =
  let q' = (q / 2) + 0.5
  in (p * f (1 - q'), p * f q')

{- | Linear pan.

>>> map (lin_pan2 1) [-1,-0.5,0,0.5,1]
[(1.0,0.0),(0.75,0.25),(0.5,0.5),(0.25,0.75),(0.0,1.0)]
-}
lin_pan2 :: Fractional t => t -> t -> (t, t)
lin_pan2 = pan2_f id

{- | Equal power pan.

>>> map (eq_pan2 1) [-1,-0.5,0,0.5,1]
[(1.0,0.0),(0.8660254037844386,0.5),(0.7071067811865476,0.7071067811865476),(0.5,0.8660254037844386),(0.0,1.0)]
-}
eq_pan2 :: Floating t => t -> t -> (t, t)
eq_pan2 = pan2_f sqrt

-- | 'fromInteger' of 'properFraction'.
sc3_properFraction :: RealFrac t => t -> (t, t)
sc3_properFraction a =
  let (p, q) = properFraction a
  in (fromInteger p, q)

-- | a^2 - b^2.
sc3_dif_sqr :: Num a => a -> a -> a
sc3_dif_sqr a b = (a * a) - (b * b)

-- | Euclidean distance function ('sqrt' of sum of squares).
sc3_hypot :: Floating a => a -> a -> a
sc3_hypot x y = sqrt (x * x + y * y)

-- | Sc3 hypotenuse approximation function.
sc3_hypotx :: (Ord a, Floating a) => a -> a -> a
sc3_hypotx x y = abs x + abs y - ((sqrt 2 - 1) * min (abs x) (abs y))

{- | Fold /k/ to within range /(i,j)/, ie. @AbstractFunction.fold@

>>> map (foldToRange 5 10) [3..12]
[7,6,5,6,7,8,9,10,9,8]
-}
foldToRange :: (Ord a, Num a) => a -> a -> a -> a
foldToRange i j =
  let f n =
        if n > j
          then f (j - (n - j))
          else
            if n < i
              then f (i - (n - i))
              else n
  in f

-- | Variant of 'foldToRange' with @Sc3@ argument ordering.
sc3_fold :: (Ord a, Num a) => a -> a -> a -> a
sc3_fold n i j = foldToRange i j n

-- | Sc3 distort operator.
sc3_distort :: Fractional n => n -> n
sc3_distort x = x / (1 + abs x)

-- | Sc3 softclip operator.
sc3_softclip :: (Ord n, Fractional n) => n -> n
sc3_softclip x = let x' = abs x in if x' <= 0.5 then x else (x' - 0.25) / x

-- * Bool

-- | True is conventionally 1.  The test to determine true is @> 0@.
sc3_true :: Num n => n
sc3_true = 1

-- | False is conventionally 0.  The test to determine true is @<= 0@.
sc3_false :: Num n => n
sc3_false = 0

{- | Lifted 'not'.

>>> sc3_not sc3_true == sc3_false
True

>>> sc3_not sc3_false == sc3_true
True
-}
sc3_not :: (Ord n, Num n) => n -> n
sc3_not = sc3_bool . (<= 0)

-- | Translate 'Bool' to 'sc3_true' and 'sc3_false'.
sc3_bool :: Num n => Bool -> n
sc3_bool b = if b then sc3_true else sc3_false

-- | Lift comparison function.
sc3_comparison :: Num n => (n -> n -> Bool) -> n -> n -> n
sc3_comparison f p q = sc3_bool (f p q)

-- * Eq

-- | Lifted '=='.
sc3_eq :: (Num n, Eq n) => n -> n -> n
sc3_eq = sc3_comparison (==)

-- | Lifted '/='.
sc3_neq :: (Num n, Eq n) => n -> n -> n
sc3_neq = sc3_comparison (/=)

-- * Ord

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

-- * Clip Rule

-- | Enumeration of clipping rules.
data Clip_Rule = Clip_None | Clip_Left | Clip_Right | Clip_Both
  deriving (Enum, Bounded)

{- | Clip a value that is expected to be within an input range to an output range, according to a rule.

>>> let f r = map (\x -> apply_clip_rule r 0 1 (-1) 1 x) [-1,0,0.5,1,2]
>>> map f [minBound .. maxBound]
[[Nothing,Nothing,Nothing,Nothing,Nothing],[Just (-1.0),Just (-1.0),Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Just 1.0,Just 1.0],[Just (-1.0),Just (-1.0),Nothing,Just 1.0,Just 1.0]]
-}
apply_clip_rule :: Ord n => Clip_Rule -> n -> n -> n -> n -> n -> Maybe n
apply_clip_rule clip_rule sl sr dl dr x =
  case clip_rule of
    Clip_None -> Nothing
    Clip_Left -> if x <= sl then Just dl else Nothing
    Clip_Right -> if x >= sr then Just dr else Nothing
    Clip_Both -> if x <= sl then Just dl else if x >= sr then Just dr else Nothing

-- * LinLin

-- | Scale uni-polar (0,1) input to linear (l,r) range.
urange_ma :: Fractional a => Sc3_MulAdd a -> a -> a -> a -> a
urange_ma mul_add_f l r i = mul_add_f i (r - l) l

{- | Scale (0,1) input to linear (l,r) range. u = uni-polar.

>>> map (urange 3 4) [0,0.5,1]
[3.0,3.5,4.0]
-}
urange :: Fractional a => a -> a -> a -> a
urange = urange_ma sc3_mul_add

{- | Calculate multiplier and add values for (-1,1) 'range' transform.

>>> range_muladd 3 4
(0.5,3.5)
-}
range_muladd :: Fractional t => t -> t -> (t, t)
range_muladd = linlin_muladd (-1) 1

{- | Scale bi-polar (-1,1) input to linear (l,r) range.
Note that the argument order is not the same as 'linLin'.
-}
range_ma :: Fractional a => Sc3_MulAdd a -> a -> a -> a -> a
range_ma mul_add_f l r i =
  let (m, a) = range_muladd l r
  in mul_add_f i m a

{- | Scale (-1,1) input to linear (l,r) range.
Note that the argument order is not the same as 'linlin'.
Note also that the various range Ugen methods at sclang select mul-add values given the output range of the Ugen, ie LFPulse.range selects a (0,1) input range.

>>> map (range 3 4) [-1,0,1]
[3.0,3.5,4.0]

>>> map (\x -> let (m,a) = linlin_muladd (-1) 1 3 4 in x * m + a) [-1,0,1]
[3.0,3.5,4.0]
-}
range :: Fractional a => a -> a -> a -> a
range = range_ma sc3_mul_add

-- | 'uncurry' 'range'
range_hs :: Fractional a => (a, a) -> a -> a
range_hs = uncurry range

-- | 'flip' 'range_hs'.  This allows cases such as osc `in_range` (0,1)
in_range :: Fractional a => a -> (a, a) -> a
in_range = flip range_hs

{- | Calculate multiplier and add values for 'linlin' transform.
Inputs are: input-min input-max output-min output-max

>>> range_muladd 3 4
(0.5,3.5)

>>> linlin_muladd (-1) 1 3 4
(0.5,3.5)

>>> linlin_muladd 0 1 3 4
(1.0,3.0)

>>> linlin_muladd (-1) 1 0 1
(0.5,0.5)

>>> linlin_muladd (-0.3) 1 (-1) 1
(1.5384615384615383,-0.5384615384615385)
-}
linlin_muladd :: Fractional t => t -> t -> t -> t -> (t, t)
linlin_muladd sl sr dl dr =
  let m = (dr - dl) / (sr - sl)
      a = dl - (m * sl)
  in (m, a)

{- | Map from one linear range to another linear range.

>>> linlin_ma sc3_mul_add 5 0 10 (-1) 1
0.0
-}
linlin_ma :: Fractional a => Sc3_MulAdd a -> a -> a -> a -> a -> a -> a
linlin_ma mul_add_f i sl sr dl dr =
  let (m, a) = linlin_muladd sl sr dl dr
  in mul_add_f i m a

{- | 'linLin' with a more typical haskell argument structure, ranges as pairs and input last.

>>> map (linlin_hs (0,127) (-0.5,0.5)) [0,63.5,127]
[-0.5,0.0,0.5]
-}
linlin_hs :: Fractional a => (a, a) -> (a, a) -> a -> a
linlin_hs (sl, sr) (dl, dr) = let (m, a) = linlin_muladd sl sr dl dr in (+ a) . (* m)

{- | Map from one linear range to another linear range.

>>> map (\i -> sc3_linlin i (-1) 1 0 1) [-1,-0.75 .. 1]
[0.0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1.0]
-}
sc3_linlin :: Fractional a => a -> a -> a -> a -> a -> a
sc3_linlin i sl sr dl dr = linlin_hs (sl, sr) (dl, dr) i

{- | Given enumeration from /dst/ that is in the same relation as /n/ is from /src/.

>>> linlin_enum_plain 'a' 'A' 'e'
'E'

>>> linlin_enum_plain 0 (-50) 16
-34

>>> linlin_enum_plain 0 (-50) (-1)
-51
-}
linlin_enum_plain :: (Enum t, Enum u) => t -> u -> t -> u
linlin_enum_plain src dst n = toEnum (fromEnum dst + (fromEnum n - fromEnum src))

{- | Variant of 'linlin_enum_plain' that requires /src/ and /dst/ ranges to be of equal size,
-- and for /n/ to lie in /src/.

>>> linlin_enum (0,100) (-50,50) 0x10 -- 16
Just (-34)

>>> linlin_enum (-50,50) (0,100) (-34)
Just 16

>>> linlin_enum (0,100) (-50,50) (-1)
Nothing
-}
linlin_enum :: (Enum t, Enum u) => (t, t) -> (u, u) -> t -> Maybe u
linlin_enum (l, r) (l', r') n =
  if fromEnum n >= fromEnum l && fromEnum r - fromEnum l == fromEnum r' - fromEnum l'
    then Just (linlin_enum_plain l l' n)
    else Nothing

-- | Erroring variant.
linlin_enum_err :: (Enum t, Enum u) => (t, t) -> (u, u) -> t -> u
linlin_enum_err src dst = fromMaybe (error "linlin_enum") . linlin_enum src dst

{- | Variant of 'linlin' that requires /src/ and /dst/ ranges to be of
-- equal size, thus with constraint of 'Num' and 'Eq' instead of
-- 'Fractional'.

>>> linlin_eq (0,100) (-50,50) 0x10 -- 16
Just (-34)

>>> linlin_eq (-50,50) (0,100) (-34)
Just 16
-}
linlin_eq :: (Eq a, Num a) => (a, a) -> (a, a) -> a -> Maybe a
linlin_eq (l, r) (l', r') n =
  let d = r - l
      d' = r' - l'
  in if d == d' then Just (l' + (n - l)) else Nothing

-- | Erroring variant.
linlin_eq_err :: (Eq a, Num a) => (a, a) -> (a, a) -> a -> a
linlin_eq_err src dst = fromMaybe (error "linlin_eq") . linlin_eq src dst

-- * LinExp

{- | Linear to exponential range conversion.
     Rule is as at linExp Ugen, haskell manner argument ordering.
     Destination values must be nonzero and have the same sign.

>>> map (floor . linexp_hs (1,2) (10,100)) [0,1,1.5,2,3]
[1,10,31,100,1000]

>>> map (floor . linexp_hs (-2,2) (1,100)) [-3,-2,-1,0,1,2,3]
[0,1,3,10,31,100,316]
-}
linexp_hs :: Floating a => (a, a) -> (a, a) -> a -> a
linexp_hs (in_l, in_r) (out_l, out_r) x =
  let rt = out_r / out_l
      rn = 1.0 / (in_r - in_l)
      rr = rn * negate in_l
  in out_l * (rt ** (x * rn + rr))

{- | Variant of 'linexp_hs' with argument ordering as at 'linExp' Ugen.

>>> map (\i -> lin_exp i 1 2 1 3) [1,1.1,1.9,2]
[1.0,1.1161231740339046,2.6878753795222865,3.0]

>>> map (\i -> floor (lin_exp i 1 2 10 100)) [0,1,1.5,2,3]
[1,10,31,100,1000]
-}
lin_exp :: Floating a => a -> a -> a -> a -> a -> a
lin_exp x in_l in_r out_l out_r = linexp_hs (in_l, in_r) (out_l, out_r) x

{- | @SimpleNumber.linexp@ shifts from linear to exponential ranges.

>>> map (sc3_linexp 1 2 1 3) [1,1.1,1.9,2]
[1.0,1.1161231740339046,2.6878753795222865,3.0]

> [1,1.5,2].collect({|i| i.linexp(1,2,10,100).floor}) == [10,31,100]

>>> map (floor . sc3_linexp 1 2 10 100) [0,1,1.5,2,3]
[10,10,31,100,100]
-}
sc3_linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
sc3_linexp src_l src_r dst_l dst_r x =
  case apply_clip_rule Clip_Both src_l src_r dst_l dst_r x of
    Just r -> r
    Nothing -> ((dst_r / dst_l) ** ((x - src_l) / (src_r - src_l))) * dst_l

{- | @SimpleNumber.explin@ is the inverse of linexp.

>>> map (sc3_explin 10 100 1 2) [10,10,31,100,100]
[1.0,1.0,1.4913616938342726,2.0,2.0]
-}
sc3_explin :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
sc3_explin src_l src_r dst_l dst_r x =
  fromMaybe
    (logBase (src_r / src_l) (x / src_l) * (dst_r - dst_l) + dst_l)
    (apply_clip_rule Clip_Both src_l src_r dst_l dst_r x)

-- * ExpExp

{- | Translate from one exponential range to another.

>>> map (round . sc3_expexp 0.1 10 4.3 100) [1 .. 10]
[21,33,44,53,62,71,78,86,93,100]
-}
sc3_expexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
sc3_expexp src_l src_r dst_l dst_r x =
  fromMaybe
    ((dst_r / dst_l) ** logBase (src_r / src_l) (x / src_l) * dst_l)
    (apply_clip_rule Clip_Both src_l src_r dst_l dst_r x)

-- * LinCurve

{- | Map /x/ from an assumed linear input range (src_l,src_r) to an
exponential curve output range (dst_l,dst_r). 'curve' is like the
parameter in Env.  Unlike with linexp, the output range may include
zero.

> (0..10).lincurve(0,10,-4.3,100,-3).round == [-4,24,45,61,72,81,87,92,96,98,100]

>>> let f = round . sc3_lincurve (-3) 0 10 (-4.3) 100
>>> map f [0 .. 10]
[-4,24,45,61,72,81,87,92,96,98,100]

> import Sound.Sc3.Plot {\- hsc3-plot -\}
> plotTable (map (\c-> map (sc3_lincurve c 0 1 (-1) 1) [0,0.01 .. 1]) [-6,-4 .. 6])
-}
sc3_lincurve :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a -> a
sc3_lincurve curve src_l src_r dst_l dst_r x =
  case apply_clip_rule Clip_Both src_l src_r dst_l dst_r x of
    Just r -> r
    Nothing ->
      if abs curve < 0.001
        then linlin_hs (src_l, src_r) (dst_l, dst_r) x
        else
          let grow = exp curve
              a = (dst_r - dst_l) / (1.0 - grow)
              b = dst_l + a
              scaled = (x - src_l) / (src_r - src_l)
          in b - (a * (grow ** scaled))

{- | Inverse of 'sc3_lincurve'.

>>> let f = round . sc3_curvelin (-3) (-4.3) 100 0 10
>>> map f [-4,24,45,61,72,81,87,92,96,98,100] == [0..10]
True
-}
sc3_curvelin :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a -> a
sc3_curvelin curve src_l src_r dst_l dst_r x =
  case apply_clip_rule Clip_Both src_l src_r dst_l dst_r x of
    Just r -> r
    Nothing ->
      if abs curve < 0.001
        then linlin_hs (src_l, src_r) (dst_l, dst_r) x
        else
          let grow = exp curve
              a = (src_r - src_l) / (1.0 - grow)
              b = src_l + a
          in log ((b - x) / a) * (dst_r - dst_l) / curve + dst_l

-- * Pp (pretty print)

-- | Removes all but the last trailing zero from floating point string.
double_pp_rm0 :: String -> String
double_pp_rm0 =
  let rev_f f = reverse . f . reverse
      remv l = case l of
        '0' : '.' : _ -> l
        '0' : l' -> remv l'
        _ -> l
  in rev_f remv

{- | The default show is odd, 0.05 shows as 5.0e-2.

>>> unwords (map (double_pp 4) [0.0001,0.001,0.01,0.1,1.0])
"0.0001 0.001 0.01 0.1 1.0"
-}
double_pp :: Int -> Double -> String
double_pp k n = double_pp_rm0 (Numeric.showFFloat (Just k) n "")

{- | Print as integer if integral, else as real.

>>> unwords (map (real_pp 5) [0.0001,0.001,0.01,0.1,1.0])
"0.0001 0.001 0.01 0.1 1"
-}
real_pp :: Int -> Double -> String
real_pp k n =
  let r = toRational n
  in if denominator r == 1 then show (numerator r) else double_pp k n

-- * Parser

-- | Type-specialised 'Text.Read.readMaybe'.
parse_double :: String -> Maybe Double
parse_double = Common.Base.reads_exact

-- * Optimiser

-- | Non-specialised optimised sum function (3 & 4 element adders).
sum_opt_f :: Num t => (t -> t -> t -> t) -> (t -> t -> t -> t -> t) -> [t] -> t
sum_opt_f f3 f4 =
  let recur l =
        case l of
          p : q : r : s : l' -> recur (f4 p q r s : l')
          p : q : r : l' -> recur (f3 p q r : l')
          _ -> sum l
  in recur

-- * Sin

{- | Taylor approximation of sin, (-pi, pi).

> import Sound.Sc3.Plot
> let xs = [-pi, -pi + 0.05 .. pi] in plot_p1_ln [map sin_taylor_approximation xs, map sin xs]
> let xs = [-pi, -pi + 0.05 .. pi] in plot_p1_ln [map (\x -> sin_taylor_approximation x - sin x) xs]
-}
sin_taylor_approximation :: Floating a => a -> a
sin_taylor_approximation x = x - (x ** 3) / (3 * 2) + (x ** 5) / (5 * 4 * 3 * 2) - (x ** 7) / (7 * 6 * 5 * 4 * 3 * 2) + (x ** 9) / (9 * 8 * 7 * 6 * 5 * 4 * 3 * 2)

{- | Bhaskara approximation of sin, (0, pi).

> import Sound.Sc3.Plot
> let xs = [0, 0.05 .. pi] in plot_p1_ln [map sin_bhaskara_approximation xs, map sin xs]
> let xs = [0, 0.05 .. pi] in plot_p1_ln [map (\x -> sin_bhaskara_approximation x - sin x) xs]
-}
sin_bhaskara_approximation :: Floating a => a -> a
sin_bhaskara_approximation x = (16 * x * (pi - x)) / ((5 * pi * pi) - (4 * x * (pi - x)))

{- | Robin Green, robin_green@playstation.sony.com, (-pi, pi)

> import Sound.Sc3.Plot
> let xs = [-pi, -pi + 0.05 .. pi] in plot_p1_ln [map sin_green_approximation xs, map sin xs]
> let xs = [-pi, -pi + 0.05 .. pi] in plot_p1_ln [map (\x -> sin_green_approximation x - sin x) xs]
-}
sin_green_approximation :: Floating a => a -> a
sin_green_approximation x = x - (0.166666546 * (x ** 3)) + (0.00833216076 * (x ** 5)) - (0.000195152832 * (x ** 7))

{- | Paul Adenot, (-pi, pi)

> import Sound.Sc3.Plot
> let xs = [-pi, -pi + 0.05 .. pi] in plot_p1_ln [map sin_adenot_approximation xs, map sin xs]
> let xs = [-pi, -pi + 0.05 .. pi] in plot_p1_ln [map (\x -> sin_adenot_approximation x - sin x) xs]
-}
sin_adenot_approximation :: Floating a => a -> a
sin_adenot_approximation x = 0.391969947653056 * x * (pi - abs x)

{- Anthony C. Robin, (0, 45).
Simple Trigonometric Approximations, The Mathematical Gazette, Vol. 79, No. 485 (Jul., 1995), pp. 385-387.

> import Sound.Sc3.Plot
> degrees_to_radians = (* pi) . (/ 180)
> let xs = [0, 0.05 .. 45] in plot_p1_ln [map sin_robin_approximation xs, map (sin . degrees_to_radians) xs]
> let xs = [0, 0.05 .. 45] in plot_p1_ln [map (\x -> sin_robin_approximation x - sin (degrees_to_radians x)) xs]
-}
sin_robin_approximation :: Floating a => a -> a
sin_robin_approximation x = let c = x * 0.01 in c * (2 - c)
