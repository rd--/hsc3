module Sound.SC3.Common.Math where

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

sc_truncate :: (RealFrac a, Num a) => a -> a
sc_truncate = fromInteger . truncate

sc_round :: (RealFrac a, Num a) => a -> a
sc_round = fromInteger . round

sc_ceiling :: (RealFrac a, Num a) => a -> a
sc_ceiling = fromInteger . ceiling

sc_floor :: (RealFrac a, Num a) => a -> a
sc_floor = fromInteger . floor

-- | Clip /n/ to within range /(i,j)/.  'clip' is a 'UGen', hence prime.
--
-- > map (clip 5 10) [3..12] == [5,5,5,6,7,8,9,10,10,10]
clip' :: (Ord a) => a -> a -> a -> a
clip' i j n = if n < i then i else if n > j then j else n

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
-- > midi_to_cps 69 == 440
midi_to_cps :: Floating a => a -> a
midi_to_cps i = 440.0 * (2.0 ** ((i - 69.0) * (1.0 / 12.0)))

cps_to_midi :: Floating a => a -> a
cps_to_midi a = (logBase 2 (a * (1.0 / 440.0)) * 12.0) + 69.0

cps_to_oct :: Floating a => a -> a
cps_to_oct a = logBase 2 (a * (1.0 / 440.0)) + 4.75

amp_to_db :: Floating a => a -> a
amp_to_db a = logBase 10 a * 20

db_to_amp :: Floating a => a -> a
db_to_amp a = 10 ** (a * 0.05)

midi_to_ratio :: Floating a => a -> a
midi_to_ratio a = 2.0 ** (a * (1.0 / 12.0))

oct_to_cps :: Floating a => a -> a
oct_to_cps a = 440.0 * (2.0 ** (a - 4.75))

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

-- | Exponential range conversion.
--
-- > map (\i -> lin_exp i 1 2 1 3) [1,1.1 .. 2]
lin_exp :: Floating a => a -> a -> a -> a -> a -> a
lin_exp i in_l in_r out_l out_r =
    let rt = out_r / out_l
        rn = 1.0 / (in_r - in_l)
        rr = rn * negate in_l
    in out_l * (rt ** (i * rn + rr))

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
