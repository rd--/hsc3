-- | Implementaion of server b_gen routines.
--
-- The naming scheme is: _p generates one partial, _l generates a list
-- of partials, _nrm is the unit normalised form.
module Sound.SC3.Common.Buffer.Gen where

import Data.List {- base -}

import Sound.SC3.Common.Buffer {- hsc3 -}
import Sound.SC3.Common.Math {- hsc3 -}

-- | Sum (mix) multiple tables into one.
sum_l :: Num n => [[n]] -> [n]
sum_l = map sum . transpose

-- | Unit normalisation.
nrm_u :: (Fractional n,Ord n) => [n] -> [n]
nrm_u = normalize (-1) 1

-- * sine1

sine1_p :: (Enum n,Floating n) => Int -> (n,n) -> [n]
sine1_p n (pfreq,ampl) = sine3_p n (pfreq,ampl,0)

sine1_l :: (Enum n,Floating n) => Int -> [n] -> [[n]]
sine1_l n ampl = map (sine1_p n) (zip [1..] ampl)

-- > import Sound.SC3.Plot
-- > plotTable1 (sine1 256 [1,0.95 .. 0.5])
sine1 :: (Enum n,Floating n) => Int -> [n] -> [n]
sine1 n = sum_l . sine1_l n

-- > plotTable1 (sine1_nrm 256 [1,0.95 .. 0.5])
sine1_nrm :: (Enum n,Floating n,Ord n) => Int -> [n] -> [n]
sine1_nrm n = nrm_u . sine1 n

-- * sine2

sine2_l :: (Enum n,Floating n) => Int -> [(n,n)] -> [[n]]
sine2_l n = map (sine1_p n)

-- > plotTable1 (sine2 256 (zip [1,2..] [1,0.95 .. 0.5]))
-- > plotTable1 (sine2 256 (zip [1,1.5 ..] [1,0.95 .. 0.5]))
sine2 :: (Enum n,Floating n) => Int -> [(n,n)] -> [n]
sine2 n = sum_l . sine2_l n

sine2_nrm :: (Enum n,Floating n,Ord n) => Int -> [n] -> [n]
sine2_nrm n = nrm_u . sine1 n

-- * sine3

sine3_p :: (Enum n,Floating n) => Int -> (n,n,n) -> [n]
sine3_p n (pfreq,ampl,phase) =
    let incr = (two_pi / (fromIntegral n - 1)) * pfreq
    in map ((*) ampl . sin) (take n [phase,phase + incr ..])

sine3_l :: (Enum n,Floating n) => Int -> [(n,n,n)] -> [[n]]
sine3_l n = map (sine3_p n)

-- > plotTable1 (sine3 256 (zip3 [1,1.5 ..] [1,0.95 .. 0.5] [0,pi/7..]))
sine3 :: (Enum n,Floating n) => Int -> [(n,n,n)] -> [n]
sine3 n = sum_l . sine3_l n

-- * cheby

{- | Generate Chebyshev waveshaping table, see b_gen_cheby.

> import Sound.SC3.Plot
> plotTable1 (gen_cheby 256 [1,0,1,1,0,1])

-}
gen_cheby :: (Enum n, Floating n, Ord n, Integral i) => i -> [n] -> [n]
gen_cheby n =
    let acos' x = if x > 1 then 0 else if x < -1 then pi else acos x
        c k x = cos (k * acos' x)
        ix = [-1,-1 + (2 / (fromIntegral n - 1)) .. 1]
        mix = map sum . transpose
        c_normalize x = let m = maximum (map abs x) in map (* (recip m)) x
    in c_normalize . mix . map (\(k,a) -> map ((* a) . (c k)) ix) . zip [1..]

cheby :: (Enum n, Floating n, Ord n) => Int -> [n] -> [n]
cheby = gen_cheby
