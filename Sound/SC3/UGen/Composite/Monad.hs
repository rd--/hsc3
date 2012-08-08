-- | Monad constructors for composite 'UGen's.
module Sound.SC3.UGen.Composite.Monad where

import qualified Sound.SC3.UGen.Composite.ID as C
import Sound.SC3.UGen.Demand.Monad
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Noise.Monad
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Lift
import Sound.SC3.UGen.UId

-- | Demand rate (:) function.
dcons :: (UId m) => UGen -> UGen -> m UGen
dcons x xs = do
  i <- dseq 1 (mce2 0 1)
  a <- dseq 1 (mce2 x xs)
  dswitch i a

-- | 'liftU' of 'C.choose'.
choose :: UId m => UGen -> m UGen
choose = liftU C.choose

-- | 'liftU' of 'C.lchoose'.
lchoose :: UId m => [UGen] -> m UGen
lchoose = liftU C.lchoose

-- | Randomly select one of several inputs.
tChoose :: (UId m) => UGen -> UGen -> m UGen
tChoose t a = do
  r <- tIRand 0 (constant (length (mceChannels a))) t
  return (select r a)

-- | Randomly select one of several inputs (weighted).
tWChoose :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
tWChoose t a w n = do
  i <- tWindex t n w
  return (select i a)
