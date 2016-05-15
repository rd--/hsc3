module Sound.SC3.Common.Math where

import Data.Maybe {- base -}

two_pi :: Floating n => n
two_pi = 2 * pi

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
