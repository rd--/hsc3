module Sound.SC3.UGen.Noise.Unsafe where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Lift
import qualified Sound.SC3.UGen.Noise.Monadic as N

-- | Brown noise.
brownNoise :: Rate -> UGen
brownNoise = liftP N.brownNoise

-- | Clip noise.
clipNoise :: Rate -> UGen
clipNoise = liftP N.clipNoise

-- | Randomly pass or block triggers.
coinGate :: UGen -> UGen -> UGen
coinGate = liftP2 N.coinGate

-- | Random impulses in (-1, 1).
dust2 :: Rate -> UGen -> UGen
dust2 = liftP2 N.dust2

-- | Random impulse in (0,1).
dust :: Rate -> UGen -> UGen
dust = liftP2 N.dust

-- | Random value in exponential distribution.
expRand :: UGen -> UGen -> UGen
expRand = liftP2 N.expRand

-- | Gray noise.
grayNoise :: Rate -> UGen
grayNoise = liftP N.grayNoise

-- | Random integer in uniform distribution.
iRand :: UGen -> UGen -> UGen
iRand = liftP2 N.iRand

-- | Clip noise.
lfClipNoise :: Rate -> UGen -> UGen
lfClipNoise = liftP2 N.lfClipNoise

-- | Dynamic clip noise.
lfdClipNoise :: Rate -> UGen -> UGen
lfdClipNoise = liftP2 N.lfdClipNoise

-- | Dynamic step noise.
lfdNoise0 :: Rate -> UGen -> UGen
lfdNoise0 = liftP2 N.lfdNoise0

-- | Dynamic ramp noise.
lfdNoise1 :: Rate -> UGen -> UGen
lfdNoise1 = liftP2 N.lfdNoise1

-- | Dynamic quadratic noise
lfdNoise2 :: Rate -> UGen -> UGen
lfdNoise2 = liftP2 N.lfdNoise2

-- | Dynamic cubic noise
lfdNoise3 :: Rate -> UGen -> UGen
lfdNoise3 = liftP2 N.lfdNoise3

-- | Step noise.
lfNoise0 :: Rate -> UGen -> UGen
lfNoise0 = liftP2 N.lfNoise0

-- | Ramp noise.
lfNoise1 :: Rate -> UGen -> UGen
lfNoise1 = liftP2 N.lfNoise1

-- | Quadratic noise.
lfNoise2 :: Rate -> UGen -> UGen
lfNoise2 = liftP2 N.lfNoise2

-- | Random value in skewed linear distribution.
linRand :: UGen -> UGen -> UGen -> UGen
linRand = liftP3 N.linRand

-- | Random value in sum of n linear distribution.
nRand :: UGen -> UGen -> UGen -> UGen
nRand = liftP3 N.nRand

-- | Pink noise.
pinkNoise :: Rate -> UGen
pinkNoise = liftP N.pinkNoise

-- | Random value in uniform distribution.
rand :: UGen -> UGen -> UGen
rand = liftP2 N.rand

-- | Random value in exponential distribution on trigger.
tExpRand :: UGen -> UGen -> UGen -> UGen
tExpRand = liftP3 N.tExpRand

-- | Random integer in uniform distribution on trigger.
tiRand :: UGen -> UGen -> UGen -> UGen
tiRand = liftP3 N.tiRand

-- | Random value in uniform distribution on trigger.
tRand :: UGen -> UGen -> UGen -> UGen
tRand = liftP3 N.tRand

-- | Triggered windex.
twindex :: UGen -> UGen -> UGen -> UGen
twindex = liftP3 N.twindex

-- | White noise.
whiteNoise :: Rate -> UGen
whiteNoise = liftP N.whiteNoise
