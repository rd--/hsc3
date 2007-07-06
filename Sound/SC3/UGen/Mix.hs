module Sound.SC3.UGen.Mix (mix, mixFill) where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Math ()

-- | Collapse MCE by summing.
mix :: UGen -> UGen
mix (MCE u)  = foldl1 (+) u
mix u        = u

-- | Construct and sum a set of UGens.
mixFill :: Int -> (Int -> UGen) -> UGen
mixFill n f = mix (MCE (map f [0..n-1]))
