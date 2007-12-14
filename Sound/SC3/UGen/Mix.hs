module Sound.SC3.UGen.Mix (mix, mixFill, freqShift) where

import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Oscillator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Math ()

-- | Collapse MCE by summing.
mix :: UGen -> UGen
mix (MCE u)  = foldl1 (+) u
mix u        = u

-- | Construct and sum a set of UGens.
mixFill :: Int -> (Int -> UGen) -> UGen
mixFill n f = mix (MCE (map f [0..n-1]))

-- | Frequency shifter, in terms of Hilbert UGen.
freqShift :: UGen -> UGen -> UGen -> UGen
freqShift i f p = mix (h * o)
    where o = sinOsc AR f (MCE [p + 0.5 * pi, p])
          h = hilbert i
