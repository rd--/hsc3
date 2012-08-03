-- | Module exporting all of "Sound.SC3.UGen" and also the monad
-- constructor variants for non-deterministic and non-sharable unit
-- generators.
module Sound.SC3.UGen.Monad (module M,clone) where

import Control.Monad
import Sound.SC3.UGen as M
import Sound.SC3.UGen.Composite.Monad as M
import Sound.SC3.UGen.Demand.Monad as M
import Sound.SC3.UGen.FFT.Monad as M
import Sound.SC3.UGen.Noise.Monad as M

-- | Clone a unit generator (mce . replicateM).
clone :: (UId m) => Int -> m UGen -> m UGen
clone n = liftM mce . replicateM n
