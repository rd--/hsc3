module Sound.SC3.UGen.Composite.Monadic where

import Sound.SC3.UGen.Demand.Monadic
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Noise.Monadic
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UId

-- | Demand rate (:) function.
dcons :: (UId m) => UGen -> UGen -> m UGen
dcons x xs = do
  i <- dseq 1 (mce2 0 1)
  a <- dseq 1 (mce2 x xs)
  dswitch i a

tChoose :: (UId m) => UGen -> UGen -> m UGen
tChoose t a = do
  r <- tiRand 0 (constant (length (mceChannels a))) t
  return (select r a)

twChoose :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
twChoose t a w n = do
  i <- twindex t n w
  return (select i a)
