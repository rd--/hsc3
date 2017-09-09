module Sound.SC3.Common.Math where

import qualified Data.Fixed as F {- base -}
import Data.Maybe {- base -}

-- | Half pi.
--
-- > half_pi == 1.5707963267948966
half_pi :: Floating a => a
half_pi = pi / 2

-- | Two pi.
--
-- > two_pi == 6.283185307179586
two_pi :: Floating n => n
two_pi = 2 * pi

-- | Multiply and add, ordinary haskell argument order.
-- 'mul_add' is a method of the 'MulAdd' class.
--
-- > map (mul_add_hs 2 3) [1,2] == [5,7] && map (mul_add_hs 3 4) [1,2] == [7,10]
mul_add_hs :: Num a => a -> a -> a -> a
mul_add_hs m a = (+ a) . (* m)

sc_truncate :: RealFrac a => a -> a
sc_truncate = fromInteger . truncate

sc_round :: RealFrac a => a -> a
sc_round = fromInteger . round

sc_ceiling :: RealFrac a => a -> a
sc_ceiling = fromInteger . ceiling

sc_floor :: RealFrac a => a -> a
sc_floor = fromInteger . floor

-- | Variant of @SC3@ @roundTo@ function.
--
-- > let r = [0,0,0.25,0.25,0.5,0.5,0.5,0.75,0.75,1,1]
-- > in map (`sc3_round_to` 0.25) [0,0.1 .. 1] == r
sc3_round_to :: RealFrac n => n -> n -> n
sc3_round_to a b = if b == 0 then a else sc_floor ((a / b) + 0.5) * b

sc3_idiv :: RealFrac n => n -> n -> n
sc3_idiv a b = fromInteger (floor a `div` floor b)

{- | The SC3 @%@ UGen operator is the 'F.mod'' function.

> > 1.5 % 1.2 // ~= 0.3
> > -1.5 % 1.2 // ~= 0.9
> > 1.5 % -1.2 // ~= -0.9
> > -1.5 % -1.2 // ~= -0.3

> let (%) = sc3_mod
> 1.5 % 1.2 ~= 0.3
> (-1.5) % 1.2 ~= 0.9
> 1.5 % (-1.2) ~= -0.9
> (-1.5) % (-1.2) ~= -0.3

> > 1.2 % 1.5 // ~= 1.2
> > -1.2 % 1.5 // ~= 0.3
> > 1.2 % -1.5 // ~= -0.3
> > -1.2 % -1.5 // ~= -1.2

> 1.2 % 1.5 ~= 1.2
> (-1.2) % 1.5 ~= 0.3
> 1.2 % (-1.5) ~= -0.3
> (-1.2) % (-1.5) ~= -1.2

> map (\n -> sc3_mod n 12.0) [-1.0,12.25,15.0] == [11.0,0.25,3.0]
-}
sc3_mod :: RealFrac n => n -> n -> n
sc3_mod = F.mod'

-- | Type specialised 'sc3_mod'.
fmod_f32 :: Float -> Float -> Float
fmod_f32 = sc3_mod

-- | Type specialised 'sc3_mod'.
fmod_f64 :: Double -> Double -> Double
fmod_f64 = sc3_mod

-- | Clip /n/ to within range /(i,j)/.  'clip' is a 'UGen', hence prime.
--
-- > map (clip 5 10) [3..12] == [5,5,5,6,7,8,9,10,10,10]
clip' :: (Ord a) => a -> a -> a -> a
clip' i j n = if n < i then i else if n > j then j else n

-- | Variant of 'clip'' with @SC3@ argument ordering.
clip_ :: Ord a => a -> a -> a -> a
clip_ n i j = clip' i j n

-- | Fractional modulo.
--
-- > map (\n -> sc_mod n 12.0) [-1.0,12.25,15.0] == [11.0,0.25,3.0]
sc_mod :: RealFrac a => a -> a -> a
sc_mod n hi =
    let lo = 0.0
    in if n >= lo && n < hi
       then n
       else if hi == lo
            then lo
            else n - hi * sc_floor (n / hi)

{- | Wrap function that is /non-inclusive/ at right edge.

> map (sc_wrap 0 5) [4,5,6] == [4.0,0.0,1.0]
> map (sc_wrap 5 10) [3..12] == [8.0,9.0,5.0,6.0,7.0,8.0,9.0,5.0,6.0,7.0]
-}
sc_wrap :: RealFrac a => a -> a -> a -> a
sc_wrap lo hi n = sc_mod (n - lo) (hi - lo) + lo

wrap_rh :: RealFrac n => (n -> n -> Bool) -> n -> n -> n -> n
wrap_rh f i j n =
    let r = j - i + 1
    in if n >= i && n `f` j
       then n
       else n - r * sc_floor ((n - i) / r)

{- | Wrap /n/ to within range /(i,j)/, ie. @AbstractFunction.wrap@,
ie. /inclusive/ at right edge.  'wrap' is a 'UGen', hence prime.

> > [5,6].wrap(0,5) == [5,0]
> map (wrap' 0 5) [5,6] == [5,0]

> > [9,10,5,6,7,8,9,10,5,6].wrap(5,10) == [9,10,5,6,7,8,9,10,5,6]
> map (wrap' 5 10) [3..12] == [9,10,5,6,7,8,9,10,5,6]
-}
wrap' :: RealFrac n => n -> n -> n -> n
wrap' = wrap_rh (<=)

-- | Variant of 'wrap'' with @SC3@ argument ordering.
--
-- > map (\n -> wrap_ n 5 10) [3..12] == map (wrap' 5 10) [3..12]
wrap_ :: RealFrac n => n -> n -> n -> n
wrap_ a b c = wrap' b c a

{- | Generic variant of 'wrap''.

> > [5,6].wrap(0,5) == [5,0]
> map (generic_wrap 0 5) [5,6] == [5,0]

> > [9,10,5,6,7,8,9,10,5,6].wrap(5,10) == [9,10,5,6,7,8,9,10,5,6]
> map (generic_wrap (5::Integer) 10) [3..12] == [9,10,5,6,7,8,9,10,5,6]
-}
generic_wrap :: (Ord a, Num a) => a -> a -> a -> a
generic_wrap l r n =
    let d = r - l + 1
        f = generic_wrap l r
    in if n < l
       then f (n + d)
       else if n > r then f (n - d) else n

-- > bin_to_freq 44100 2048 32 == 689.0625
bin_to_freq :: (Fractional n, Integral i) => n -> i -> i -> n
bin_to_freq sr n i = fromIntegral i * sr / fromIntegral n

-- | Midi note number to cycles per second.
--
-- > map (floor . midi_to_cps) [0,24,69,120,127] == [8,32,440,8372,12543]
-- > map (floor . midi_to_cps) [-36,138] == [1,23679]
midi_to_cps :: Floating a => a -> a
midi_to_cps i = 440.0 * (2.0 ** ((i - 69.0) * (1.0 / 12.0)))

-- | Cycles per second to midi note number.
--
-- > map (round . cps_to_midi) [8,32,440,8372,12543] == [0,24,69,120,127]
-- > map (round . cps_to_midi) [1,24000] == [-36,138]
cps_to_midi :: Floating a => a -> a
cps_to_midi a = (logBase 2 (a * (1.0 / 440.0)) * 12.0) + 69.0

cps_to_oct :: Floating a => a -> a
cps_to_oct a = logBase 2 (a * (1.0 / 440.0)) + 4.75

oct_to_cps :: Floating a => a -> a
oct_to_cps a = 440.0 * (2.0 ** (a - 4.75))

-- | Linear amplitude to decibels.
--
-- > map (round . amp_to_db) [0.01,0.05,0.0625,0.125,0.25,0.5] == [-40,-26,-24,-18,-12,-6]
amp_to_db :: Floating a => a -> a
amp_to_db a = logBase 10 a * 20

-- | Decibels to linear amplitude.
--
-- > map (floor . (* 100). db_to_amp) [-40,-26,-24,-18,-12,-6] == [01,05,06,12,25,50]
db_to_amp :: Floating a => a -> a
db_to_amp a = 10 ** (a * 0.05)

-- | Fractional midi note interval to frequency multiplier.
--
-- > map midi_to_ratio [0,7,12] == [1,1.4983070768766815,2]
midi_to_ratio :: Floating a => a -> a
midi_to_ratio a = 2.0 ** (a * (1.0 / 12.0))

-- | Inverse of 'midi_to_ratio'.
--
-- > map ratio_to_midi [3/2,2] == [7.019550008653875,12]
ratio_to_midi :: Floating a => a -> a
ratio_to_midi a = 12.0 * logBase 2 a

-- | Scale uni-polar (0,1) input to linear (l,r) range
--
-- > map (urange 3 4) [0,0.5,1] == [3,3.5,4]
urange :: Fractional a => a -> a -> a -> a
urange l r i = let m = r - l in i * m + l

-- | Calculate multiplier and add values for 'range' transform.
--
-- > range_muladd 3 4 == (0.5,3.5)
range_muladd :: Fractional t => t -> t -> (t,t)
range_muladd = linlin_muladd (-1) 1

-- | Scale bi-polar (-1,1) input to linear (l,r) range.  Note that the
-- argument order is not the same as 'linlin'.
--
-- > map (range 3 4) [-1,0,1] == [3,3.5,4]
-- > map (\x -> let (m,a) = linlin_muladd (-1) 1 3 4 in x * m + a) [-1,0,1] == [3,3.5,4]
range :: Fractional a => a -> a -> a -> a
range l r i = let (m,a) = range_muladd l r in i * m + a

range_hs :: Fractional a => (a,a) -> a -> a
range_hs (l,r) = range l r

data Clip_Rule = Clip_None | Clip_Left | Clip_Right | Clip_Both
                 deriving (Enum,Bounded)

-- > let f r = map (\x -> apply_clip_rule r 0 1 (-1) 1 x) [-1,0,0.5,1,2]
-- > in map f [minBound .. maxBound]
apply_clip_rule :: Ord n => Clip_Rule -> n -> n -> n -> n -> n -> Maybe n
apply_clip_rule clip_rule sl sr dl dr x =
    case clip_rule of
      Clip_None -> Nothing
      Clip_Left -> if x <= sl then Just dl else Nothing
      Clip_Right -> if x >= sr then Just dr else Nothing
      Clip_Both -> if x <= sl then Just dl else if x >= sr then Just dr else Nothing

-- | Calculate multiplier and add values for 'linlin' transform.
--
-- > range_muladd 3 4 == (0.5,3.5)
-- > linlin_muladd (-1) 1 3 4 == (0.5,3.5)
-- > linlin_muladd 0 1 3 4 == (1,3)
-- > linlin_muladd (-1) 1 0 1 == (0.5,0.5)
-- > linlin_muladd (-0.3) 1 (-1) 1
linlin_muladd :: Fractional t => t -> t -> t -> t -> (t,t)
linlin_muladd sl sr dl dr =
    let m = (dr - dl) / (sr - sl)
        a = dl - (m * sl)
    in (m,a)

-- | Map from one linear range to another linear range.
--
-- > map (\i -> linlin i (-1) 1 0 1) [-1,-0.9 .. 1.0]
linlin :: Fractional a => a -> a -> a -> a -> a -> a
linlin i sl sr dl dr = let (m,a) = linlin_muladd sl sr dl dr in i * m + a

-- | Variant with a more typical argument structure, ranges as pairs and input last.
--
-- > map (linlin_hs (0,127) (-0.5,0.5)) [0,63.5,127]
linlin_hs :: Fractional a => (a, a) -> (a, a) -> a -> a
linlin_hs (sl,sr) (dl,dr) i = linlin i sl sr dl dr

-- | Given enumeration from /dst/ that is in the same relation as /n/ is from /src/.
--
-- > linlin _enum' 'a' 'A' 'e' == 'E'
-- > linlin_enum' 0 (-50) 16 == -34
-- > linlin_enum' 0 (-50) (-1) == -51
linlin_enum' :: (Enum t,Enum u) => t -> u -> t -> u
linlin_enum' src dst n = toEnum (fromEnum dst + (fromEnum n - fromEnum src))

-- | Variant of 'linlin_enum'' that requires /src/ and /dst/ ranges to be of equal size,
-- and for /n/ to lie in /src/.
--
-- > linlin_enum (0,100) (-50,50) 0x10 == Just (-34)
-- > linlin_enum (-50,50) (0,100) (-34) == Just 0x10
-- > linlin_enum (0,100) (-50,50) (-1) == Nothing
linlin_enum :: (Enum t,Enum u) => (t,t) -> (u,u) -> t -> Maybe u
linlin_enum (l,r) (l',r') n =
    if fromEnum n >= fromEnum l && fromEnum r - fromEnum l == fromEnum r' - fromEnum l'
    then Just (linlin_enum' l l' n)
    else Nothing

-- | Erroring variant.
linlin_enum_err :: (Enum t,Enum u) => (t,t) -> (u,u) -> t -> u
linlin_enum_err src dst = fromMaybe (error "linlin_enum") . linlin_enum src dst

-- | Variant of 'linlin' that requires /src/ and /dst/ ranges to be of
-- equal size, thus with constraint of 'Num' and 'Eq' instead of
-- 'Fractional'.
--
-- > linlin_eq (0,100) (-50,50) 0x10 == Just (-34)
-- > linlin_eq (-50,50) (0,100) (-34) == Just 0x10
linlin_eq :: (Eq a, Num a) => (a,a) -> (a,a) -> a -> Maybe a
linlin_eq (l,r) (l',r') n =
    let d = r - l
        d' = r' - l'
    in if d == d' then Just (l' + (n - l)) else Nothing

-- | Erroring variant.
linlin_eq_err :: (Eq a,Num a) => (a,a) -> (a,a) -> a -> a
linlin_eq_err src dst = fromMaybe (error "linlin_eq") . linlin_eq src dst

-- | @SimpleNumber.linexp@ shifts from linear to exponential ranges.
--
-- > > [1,1.5,2].collect({|i| i.linexp(1,2,10,100).floor}) == [10,31,100]
-- > map (floor . sc_linexp 1 2 10 100) [0,1,1.5,2,3] == [10,10,31,100,100]
sc_linexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
sc_linexp src_l src_r dst_l dst_r x =
    case apply_clip_rule Clip_Both src_l src_r dst_l dst_r x of
      Just r -> r
      Nothing -> ((dst_r / dst_l) ** ((x - src_l) / (src_r - src_l))) * dst_l

-- | @SimpleNumber.explin@ is the inverse of linexp.
--
-- > map (sc_explin 10 100 1 2) [10,10,31,100,100]
sc_explin :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
sc_explin src_l src_r dst_l dst_r x =
    case apply_clip_rule Clip_Both src_l src_r dst_l dst_r x of
      Just r -> r
      Nothing -> (log (x / src_l)) / (log (src_r / src_l)) * (dst_r - dst_l) + dst_l

-- > map (sc_expexp 0.1 10 4.3 100) [1.. 10]
sc_expexp :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a
sc_expexp src_l src_r dst_l dst_r x =
    case apply_clip_rule Clip_Both src_l src_r dst_l dst_r x of
      Just r -> r
      Nothing -> ((dst_r / dst_l) ** (log (x / src_l) / log (src_r / src_l))) * dst_l

-- > map (floor . linexp_hs (1,2) (10,100)) [0,1,1.5,2,3] == [1,10,31,100,1000]
linexp_hs :: Floating a => (a,a) -> (a,a) -> a -> a
linexp_hs (in_l,in_r) (out_l,out_r) x =
    let rt = out_r / out_l
        rn = 1.0 / (in_r - in_l)
        rr = rn * negate in_l
    in out_l * (rt ** (x * rn + rr))

-- | Exponential range conversion.
--
-- > map (\i -> lin_exp i 1 2 1 3) [1,1.1 .. 2]
lin_exp :: Floating a => a -> a -> a -> a -> a -> a
lin_exp x in_l in_r out_l out_r = linexp_hs (in_l,in_r) (out_l,out_r) x

-- | /sr/ = sample rate, /r/ = cycle (two-pi), /cps/ = frequency
--
-- > cps_to_incr 48000 128 375 == 1
-- > cps_to_incr 48000 two_pi 458.3662361046586 == 6e-2
cps_to_incr :: Fractional a => a -> a -> a -> a
cps_to_incr sr r cps = (r / sr) * cps

-- | Inverse of 'cps_to_incr'.
--
-- > incr_to_cps 48000 128 1 == 375
incr_to_cps :: Fractional a => a -> a -> a -> a
incr_to_cps sr r ic = ic / (r / sr)

-- | Linear pan.
--
-- > map (lin_pan2 1) [-1,0,1] == [(1,0),(0.5,0.5),(0,1)]
lin_pan2 :: Fractional t => t -> t -> (t, t)
lin_pan2 p q =
    let q' = (q / 2) + 0.5
    in (p * (1 - q'),p * q')

sc3_properFraction :: RealFrac t => t -> (t,t)
sc3_properFraction a =
    let (p,q) = properFraction a
    in (fromInteger p,q)

sc_dif_sqr :: Num a => a -> a -> a
sc_dif_sqr a b = (a * a) - (b * b)

sc_hypot :: Floating a => a -> a -> a
sc_hypot x y = sqrt (x * x + y * y)

sc_hypotx :: (Ord a, Floating a) => a -> a -> a
sc_hypotx x y = abs x + abs y - ((sqrt 2 - 1) * min (abs x) (abs y))

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

