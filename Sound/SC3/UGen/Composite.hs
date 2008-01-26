module Sound.SC3.UGen.Composite (mix, mixFill, freqShift, splay) where

import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Oscillator
import Sound.SC3.UGen.Panner
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen (UGen, mce, mceProxies)
import Sound.SC3.UGen.UGen.Math ()
import Sound.SC3.UGen.UGen.MCE
import Sound.SC3.UGen.UGen.Predicate

-- | Collapse multiple channel expansion by summing.
mix :: UGen -> UGen
mix u | isMCE u = sum (mceProxies u)
      | otherwise = u

-- | Construct and sum a set of UGens.
mixFill :: Int -> (Int -> UGen) -> UGen
mixFill n f = mix (mce (map f [0..n-1]))

-- | Frequency shifter, in terms of Hilbert UGen.
freqShift :: UGen -> UGen -> UGen -> UGen
freqShift i f p = mix (h * o)
    where o = sinOsc AR f (mce [p + 0.5 * pi, p])
          h = hilbert i

-- | Pan a set of channels across the stereo field.
splay :: UGen -> UGen -> UGen -> UGen -> UGen
splay i s l c = mix (pan2 i (mce p * s + c) 1) * l * (sqrt (1 / n))
    where n = fromIntegral (mceDegree i)
          m = n - 1
          p = map ( (+ (-1.0)) . (* (2 / m)) ) [0 .. m]
