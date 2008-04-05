module Sound.SC3.UGen.Noise.Monadic where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct
import qualified Sound.SC3.UGen.Noise.Base as N
import Sound.SC3.UGen.UId

-- | Brown noise.
brownNoise :: (UId m) => Rate -> m UGen
brownNoise = liftU N.brownNoise

-- | Clip noise.
clipNoise :: (UId m) => Rate -> m UGen
clipNoise = liftU N.clipNoise

-- | Randomly pass or block triggers.
coinGate :: (UId m) => UGen -> UGen -> m UGen
coinGate = liftU2 N.coinGate

-- | Random impulses in (-1, 1).
dust2 :: (UId m) => Rate -> UGen -> m UGen
dust2 = liftU2 N.dust2

-- | Random impulse in (0,1).
dust :: (UId m) => Rate -> UGen -> m UGen
dust = liftU2 N.dust

-- | Random value in exponential distribution.
expRand :: (UId m) => UGen -> UGen -> m UGen
expRand = liftU2 N.expRand

-- | Gray noise.
grayNoise :: (UId m) => Rate -> m UGen
grayNoise = liftU N.grayNoise

-- | Random integer in uniform distribution.
iRand :: (UId m) => UGen -> UGen -> m UGen
iRand = liftU2 N.iRand

-- | Clip noise.
lfClipNoise :: (UId m) => Rate -> UGen -> m UGen
lfClipNoise = liftU2 N.lfClipNoise

-- | Dynamic clip noise.
lfdClipNoise :: (UId m) => Rate -> UGen -> m UGen
lfdClipNoise = liftU2 N.lfdClipNoise

-- | Dynamic step noise.
lfdNoise0 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise0 = liftU2 N.lfdNoise0

-- | Dynamic ramp noise.
lfdNoise1 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise1 = liftU2 N.lfdNoise1

-- | Dynamic quadratic noise
lfdNoise2 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise2 = liftU2 N.lfdNoise2

-- | Dynamic cubic noise
lfdNoise3 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise3 = liftU2 N.lfdNoise3

-- | Step noise.
lfNoise0 :: (UId m) => Rate -> UGen -> m UGen
lfNoise0 = liftU2 N.lfNoise0

-- | Ramp noise.
lfNoise1 :: (UId m) => Rate -> UGen -> m UGen
lfNoise1 = liftU2 N.lfNoise1

-- | Quadratic noise.
lfNoise2 :: (UId m) => Rate -> UGen -> m UGen
lfNoise2 = liftU2 N.lfNoise2

-- | Random value in skewed linear distribution.
linRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
linRand = liftU3 N.linRand

-- | Random value in sum of n linear distribution.
nRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
nRand = liftU3 N.nRand

-- | Pink noise.
pinkNoise :: (UId m) => Rate -> m UGen
pinkNoise = liftU N.pinkNoise

-- | Random value in uniform distribution.
rand :: (UId m) => UGen -> UGen -> m UGen
rand = liftU2 N.rand

-- | Random value in exponential distribution on trigger.
tExpRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
tExpRand = liftU3 N.tExpRand

-- | Random integer in uniform distribution on trigger.
tiRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
tiRand = liftU3 N.tiRand

-- | Random value in uniform distribution on trigger.
tRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
tRand = liftU3 N.tRand

-- | Triggered windex.
twindex :: (UId m) => UGen -> UGen -> UGen -> m UGen
twindex = liftU3 N.twindex

-- | White noise.
whiteNoise :: (UId m) => Rate -> m UGen
whiteNoise = liftU N.whiteNoise
