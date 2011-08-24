module Sound.SC3.UGen.Composite.ID where

import Sound.SC3.Identifier
import Sound.SC3.UGen.Demand.ID
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Noise.ID
import Sound.SC3.UGen.UGen

-- | Demand rate (:) function.
dcons :: ID m => (m,m,m) -> UGen -> UGen -> UGen
dcons (z0,z1,z2) x xs =
    let i = dseq z0 1 (mce2 0 1)
        a = dseq z1 1 (mce2 x xs)
    in dswitch z2 i a

mceN :: UGen -> UGen
mceN = constant . length . mceChannels

iChoose :: ID m => m -> UGen -> UGen
iChoose e a = select (iRand e 0 (mceN a)) a

iChoose' :: ID m => m -> [UGen] -> UGen
iChoose' e = iChoose e . mce

tChoose :: ID m => m -> UGen -> UGen -> UGen
tChoose z t a = select (tiRand z 0 (mceN a) t) a

twChoose :: ID m => m -> UGen -> UGen -> UGen -> UGen -> UGen
twChoose z t a w n =
    let i = twindex z t n w
    in select i a
