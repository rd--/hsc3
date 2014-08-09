-- | Monad constructors for noise 'UGen's.
module Sound.SC3.UGen.Noise.Monad where

import Sound.SC3.UGen.Noise.ID as N
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UId

-- | Brown noise.
brownNoise :: (UId m) => Rate -> m UGen
brownNoise = liftUId N.brownNoise

-- | Clip noise.
clipNoise :: (UId m) => Rate -> m UGen
clipNoise = liftUId N.clipNoise

-- | Randomly pass or block triggers.
coinGate :: (UId m) => UGen -> UGen -> m UGen
coinGate = liftUId2 N.coinGate

-- | Random impulses in (-1, 1).
dust2 :: (UId m) => Rate -> UGen -> m UGen
dust2 = liftUId2 N.dust2

-- | Random impulse in (0,1).
dust :: (UId m) => Rate -> UGen -> m UGen
dust = liftUId2 N.dust

-- | Random value in exponential distribution.
expRand :: (UId m) => UGen -> UGen -> m UGen
expRand = liftUId2 N.expRand

-- | Gray noise.
grayNoise :: (UId m) => Rate -> m UGen
grayNoise = liftUId N.grayNoise

-- | Random integer in uniform distribution.
iRand :: (UId m) => UGen -> UGen -> m UGen
iRand = liftUId2 N.iRand

-- | Clip noise.
lfClipNoise :: (UId m) => Rate -> UGen -> m UGen
lfClipNoise = liftUId2 N.lfClipNoise

-- | Dynamic clip noise.
lfdClipNoise :: (UId m) => Rate -> UGen -> m UGen
lfdClipNoise = liftUId2 N.lfdClipNoise

-- | Dynamic step noise.
lfdNoise0 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise0 = liftUId2 N.lfdNoise0

-- | Dynamic ramp noise.
lfdNoise1 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise1 = liftUId2 N.lfdNoise1

-- | Dynamic quadratic noise
lfdNoise2 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise2 = liftUId2 N.lfdNoise2

-- | Dynamic cubic noise
lfdNoise3 :: (UId m) => Rate -> UGen -> m UGen
lfdNoise3 = liftUId2 N.lfdNoise3

-- | Step noise.
lfNoise0 :: (UId m) => Rate -> UGen -> m UGen
lfNoise0 = liftUId2 N.lfNoise0

-- | Ramp noise.
lfNoise1 :: (UId m) => Rate -> UGen -> m UGen
lfNoise1 = liftUId2 N.lfNoise1

-- | Quadratic noise.
lfNoise2 :: (UId m) => Rate -> UGen -> m UGen
lfNoise2 = liftUId2 N.lfNoise2

-- | Random value in skewed linear distribution.
linRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
linRand = liftUId3 N.linRand

-- | Random value in sum of n linear distribution.
nRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
nRand = liftUId3 N.nRand

-- | Pink noise.
pinkNoise :: (UId m) => Rate -> m UGen
pinkNoise = liftUId N.pinkNoise

-- | Random value in uniform distribution.
rand :: (UId m) => UGen -> UGen -> m UGen
rand = liftUId2 N.rand

-- | Random value in exponential distribution on trigger.
tExpRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
tExpRand = liftUId3 N.tExpRand

-- | Random integer in uniform distribution on trigger.
tIRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
tIRand = liftUId3 N.tIRand

-- | Random value in uniform distribution on trigger.
tRand :: (UId m) => UGen -> UGen -> UGen -> m UGen
tRand = liftUId3 N.tRand

-- | Triggered windex.
tWindex :: (UId m) => UGen -> UGen -> UGen -> m UGen
tWindex = liftUId3 N.tWindex

-- | White noise.
whiteNoise :: (UId m) => Rate -> m UGen
whiteNoise = liftUId N.whiteNoise
