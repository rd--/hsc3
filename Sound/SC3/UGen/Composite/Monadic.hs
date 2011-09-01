module Sound.SC3.UGen.Composite.Monadic where

import qualified Sound.SC3.UGen.Composite.ID as C
import Sound.SC3.UGen.Demand.Monadic
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Noise.Monadic
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Lift
import Sound.SC3.UGen.UId

-- | Demand rate (:) function.
dcons :: (UId m) => UGen -> UGen -> m UGen
dcons x xs = do
  i <- dseq 1 (mce2 0 1)
  a <- dseq 1 (mce2 x xs)
  dswitch i a

iChoose :: UId m => UGen -> m UGen
iChoose = liftU C.iChoose

iChoose' :: UId m => [UGen] -> m UGen
iChoose' = liftU C.iChoose'

tChoose :: (UId m) => UGen -> UGen -> m UGen
tChoose t a = do
  r <- tIRand 0 (constant (length (mceChannels a))) t
  return (select r a)

tWChoose :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
tWChoose t a w n = do
  i <- tWindex t n w
  return (select i a)
