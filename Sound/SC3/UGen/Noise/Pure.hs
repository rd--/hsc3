module Sound.SC3.UGen.Noise.Pure where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, withUId)
import Sound.SC3.UGen.UId  (UId)
import qualified Sound.SC3.UGen.Noise.Base as N

-- | Brown noise.
brownNoise :: UId -> Rate -> UGen
brownNoise d r = withUId d (N.brownNoise r)

-- | Clip noise.
clipNoise :: UId -> Rate -> UGen
clipNoise d r = withUId d (N.clipNoise r)

-- | Randomly pass or block triggers.
coinGate :: UId -> UGen -> UGen -> UGen
coinGate d prob i = withUId d (N.coinGate prob i)

-- | Random impulses in (-1  1).
dust2 :: UId -> Rate -> UGen -> UGen
dust2 d r density = withUId d (N.dust2 r density)

-- | Random impulse in (0 1).
dust :: UId -> Rate -> UGen -> UGen
dust d r density = withUId d (N.dust r density)

-- | Random value in exponential distribution.
expRand :: UId -> UGen -> UGen -> UGen
expRand d lo hi = withUId d (N.expRand lo hi)

-- | Gray noise.
grayNoise :: UId -> Rate -> UGen
grayNoise d r = withUId d (N.grayNoise r)

-- | Random integer in uniform distribution.
iRand :: UId -> UGen -> UGen -> UGen
iRand d lo hi = withUId d (N.iRand lo hi)

-- | Clip noise.
lfClipNoise :: UId -> Rate -> UGen -> UGen
lfClipNoise d r freq = withUId d (N.lfClipNoise r freq)

-- | Dynamic clip noise.
lfdClipNoise :: UId -> Rate -> UGen -> UGen
lfdClipNoise d r freq = withUId d (N.lfdClipNoise r freq)

-- | Dynamic step noise.
lfdNoise0 :: UId -> Rate -> UGen -> UGen
lfdNoise0 d r freq = withUId d (N.lfdNoise0 r freq)

-- | Dynamic ramp noise. 
lfdNoise1 :: UId -> Rate -> UGen -> UGen
lfdNoise1 d r freq = withUId d (N.lfdNoise1 r freq)

-- | Dynamic quadratic noise
lfdNoise2 :: UId -> Rate -> UGen -> UGen
lfdNoise2 d r freq = withUId d (N.lfdNoise2 r freq)

-- | Step noise.
lfNoise0 :: UId -> Rate -> UGen -> UGen
lfNoise0 d r freq = withUId d (N.lfNoise0 r freq)

-- | Ramp noise.
lfNoise1 :: UId -> Rate -> UGen -> UGen
lfNoise1 d r freq = withUId d (N.lfNoise1 r freq)

-- | Quadratic noise.
lfNoise2 :: UId -> Rate -> UGen -> UGen
lfNoise2 d r freq = withUId d (N.lfNoise2 r freq)

-- | Random value in skewed linear distribution.
linRand :: UId -> UGen -> UGen -> UGen -> UGen
linRand d lo hi m = withUId d (N.linRand lo hi m)

-- | Random value in sum of n linear distribution.
nRand :: UId -> UGen -> UGen -> UGen -> UGen
nRand d lo hi n = withUId d (N.nRand lo hi n)

-- | Pink noise.
pinkNoise :: UId -> Rate -> UGen
pinkNoise d r = withUId d (N.pinkNoise r)

-- | Random value in uniform distribution.
rand :: UId -> UGen -> UGen -> UGen
rand d lo hi = withUId d (N.rand lo hi)

-- | Random value in exponential distribution on trigger.
tExpRand :: UId -> UGen -> UGen -> UGen -> UGen
tExpRand d lo hi trig = withUId d (N.tExpRand lo hi trig)

-- | Random integer in uniform distribution on trigger.
tiRand :: UId -> UGen -> UGen -> UGen -> UGen
tiRand d lo hi trig = withUId d (N.tiRand lo hi trig)

-- | Random value in uniform distribution on trigger.
tRand :: UId -> UGen -> UGen -> UGen -> UGen
tRand d lo hi trig = withUId d (N.tRand lo hi trig)

-- | White noise.
whiteNoise :: UId -> Rate -> UGen
whiteNoise d r = withUId d (N.whiteNoise r)
