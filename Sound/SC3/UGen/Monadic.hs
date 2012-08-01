-- | Module exporting all of "Sound.SC3.UGen" and also the monadic
-- constructor variants for non-deterministic and non-sharable unit
-- generators.
module Sound.SC3.UGen.Monadic (module M,clone) where

import Control.Monad
import Sound.SC3.UGen as M
import Sound.SC3.UGen.Composite.Monadic as M
import Sound.SC3.UGen.Demand.Monadic as M
import Sound.SC3.UGen.FFT.Monadic as M
import Sound.SC3.UGen.Noise.Monadic as M

-- | Clone a unit generator (mce . replicateM).
clone :: (UId m) => Int -> m UGen -> m UGen
clone n = liftM mce . replicateM n
