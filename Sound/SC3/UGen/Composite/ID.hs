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

tChoose :: ID m => m -> UGen -> UGen -> UGen
tChoose z t a =
    let r = tiRand z 0 (constant (length (mceChannels a))) t
    in select r a

twChoose :: ID m => m -> UGen -> UGen -> UGen -> UGen -> UGen
twChoose z t a w n =
    let i = twindex z t n w
    in select i a
