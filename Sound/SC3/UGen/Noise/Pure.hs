module Sound.SC3.UGen.Noise.Pure where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import qualified Sound.SC3.UGen.Noise.Base as N

-- | Brown noise.
brownNoise :: UGenID -> Rate -> UGen
brownNoise = liftD N.brownNoise

-- | Clip noise.
clipNoise :: UGenID -> Rate -> UGen
clipNoise = liftD N.clipNoise

-- | Randomly pass or block triggers.
coinGate :: UGenID -> UGen -> UGen -> UGen
coinGate = liftD2 N.coinGate

-- | Random impulses in (-1  1).
dust2 :: UGenID -> Rate -> UGen -> UGen
dust2 = liftD2 N.dust2

-- | Random impulse in (0 1).
dust :: UGenID -> Rate -> UGen -> UGen
dust = liftD2 N.dust

-- | Random value in exponential distribution.
expRand :: UGenID -> UGen -> UGen -> UGen
expRand = liftD2 N.expRand

-- | Gray noise.
grayNoise :: UGenID -> Rate -> UGen
grayNoise = liftD N.grayNoise

-- | Random integer in uniform distribution.
iRand :: UGenID -> UGen -> UGen -> UGen
iRand = liftD2 N.iRand

-- | Clip noise.
lfClipNoise :: UGenID -> Rate -> UGen -> UGen
lfClipNoise = liftD2 N.lfClipNoise

-- | Dynamic clip noise.
lfdClipNoise :: UGenID -> Rate -> UGen -> UGen
lfdClipNoise = liftD2 N.lfdClipNoise

-- | Dynamic step noise.
lfdNoise0 :: UGenID -> Rate -> UGen -> UGen
lfdNoise0 = liftD2 N.lfdNoise0

-- | Dynamic ramp noise. 
lfdNoise1 :: UGenID -> Rate -> UGen -> UGen
lfdNoise1 = liftD2 N.lfdNoise1

-- | Dynamic quadratic noise
lfdNoise2 :: UGenID -> Rate -> UGen -> UGen
lfdNoise2 = liftD2 N.lfdNoise2

-- | Step noise.
lfNoise0 :: UGenID -> Rate -> UGen -> UGen
lfNoise0 = liftD2 N.lfNoise0

-- | Ramp noise.
lfNoise1 :: UGenID -> Rate -> UGen -> UGen
lfNoise1 = liftD2 N.lfNoise1

-- | Quadratic noise.
lfNoise2 :: UGenID -> Rate -> UGen -> UGen
lfNoise2 = liftD2 N.lfNoise2

-- | Random value in skewed linear distribution.
linRand :: UGenID -> UGen -> UGen -> UGen -> UGen
linRand = liftD3 N.linRand

-- | Random value in sum of n linear distribution.
nRand :: UGenID -> UGen -> UGen -> UGen -> UGen
nRand = liftD3 N.nRand

-- | Pink noise.
pinkNoise :: UGenID -> Rate -> UGen
pinkNoise = liftD N.pinkNoise

-- | Random value in uniform distribution.
rand :: UGenID -> UGen -> UGen -> UGen
rand = liftD2 N.rand

-- | Random value in exponential distribution on trigger.
tExpRand :: UGenID -> UGen -> UGen -> UGen -> UGen
tExpRand = liftD3 N.tExpRand

-- | Random integer in uniform distribution on trigger.
tiRand :: UGenID -> UGen -> UGen -> UGen -> UGen
tiRand = liftD3 N.tiRand

-- | Random value in uniform distribution on trigger.
tRand :: UGenID -> UGen -> UGen -> UGen -> UGen
tRand = liftD3 N.tRand

-- | White noise.
whiteNoise :: UGenID -> Rate -> UGen
whiteNoise = liftD N.whiteNoise
