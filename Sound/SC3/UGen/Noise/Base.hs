module Sound.SC3.UGen.Noise.Base where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct

-- | Brown noise.
brownNoise :: UGenId -> Rate -> UGen
brownNoise z r = mkOscId z r "BrownNoise" [] 1

-- | Clip noise.
clipNoise :: UGenId -> Rate -> UGen
clipNoise z r = mkOscId z r "ClipNoise" [] 1

-- | Randomly pass or block triggers.
coinGate :: UGenId -> UGen -> UGen -> UGen
coinGate z prob i = mkFilterId z "CoinGate" [prob, i] 1

-- | Random impulses in (-1, 1).
dust2 :: UGenId -> Rate -> UGen -> UGen
dust2 z r density = mkOscId z r "Dust2" [density] 1

-- | Random impulse in (0,1).
dust :: UGenId -> Rate -> UGen -> UGen
dust z r density = mkOscId z r "Dust" [density] 1

-- | Random value in exponential distribution.
expRand :: UGenId -> UGen -> UGen -> UGen
expRand z lo hi = mkOscId z IR "ExpRand" [lo, hi] 1

-- | Gray noise.
grayNoise :: UGenId -> Rate -> UGen
grayNoise z r = mkOscId z r "GrayNoise" [] 1

-- | Random integer in uniform distribution.
iRand :: UGenId -> UGen -> UGen -> UGen
iRand z lo hi = mkOscId z IR "IRand" [lo, hi] 1

-- | Clip noise.
lfClipNoise :: UGenId -> Rate -> UGen -> UGen
lfClipNoise z r freq = mkOscId z r "LFClipNoise" [freq] 1

-- | Dynamic clip noise.
lfdClipNoise :: UGenId -> Rate -> UGen -> UGen
lfdClipNoise z r freq = mkOscId z r "LFDClipNoise" [freq] 1

-- | Dynamic step noise.
lfdNoise0 :: UGenId -> Rate -> UGen -> UGen
lfdNoise0 z r freq = mkOscId z r "LFDNoise0" [freq] 1

-- | Dynamic ramp noise.
lfdNoise1 :: UGenId -> Rate -> UGen -> UGen
lfdNoise1 z r freq = mkOscId z r "LFDNoise1" [freq] 1

-- | Dynamic quadratic noise
lfdNoise2 :: UGenId -> Rate -> UGen -> UGen
lfdNoise2 z r freq = mkOscId z r "LFDNoise2" [freq] 1

-- | Dynamic cubic noise
lfdNoise3 :: UGenId -> Rate -> UGen -> UGen
lfdNoise3 z r freq = mkOscId z r "LFDNoise3" [freq] 1

-- | Step noise.
lfNoise0 :: UGenId -> Rate -> UGen -> UGen
lfNoise0 z r freq = mkOscId z r "LFNoise0" [freq] 1

-- | Ramp noise.
lfNoise1 :: UGenId -> Rate -> UGen -> UGen
lfNoise1 z r freq = mkOscId z r "LFNoise1" [freq] 1

-- | Quadratic noise.
lfNoise2 :: UGenId -> Rate -> UGen -> UGen
lfNoise2 z r freq = mkOscId z r "LFNoise2" [freq] 1

-- | Random value in skewed linear distribution.
linRand :: UGenId -> UGen -> UGen -> UGen -> UGen
linRand z lo hi m = mkOscId z IR "LinRand" [lo, hi, m] 1

-- | Random value in sum of n linear distribution.
nRand :: UGenId -> UGen -> UGen -> UGen -> UGen
nRand z lo hi n = mkOscId z IR "NRand" [lo, hi, n] 1

-- | Pink noise.
pinkNoise :: UGenId -> Rate -> UGen
pinkNoise z r = mkOscId z r "PinkNoise" [] 1

-- | Random value in uniform distribution.
rand :: UGenId -> UGen -> UGen -> UGen
rand z lo hi = mkOscId z IR "Rand" [lo, hi] 1

-- | Random value in exponential distribution on trigger.
tExpRand :: UGenId -> UGen -> UGen -> UGen -> UGen
tExpRand z lo hi trig = mkFilterId z "TExpRand" [lo, hi, trig] 1

-- | Random integer in uniform distribution on trigger.
tiRand :: UGenId -> UGen -> UGen -> UGen -> UGen
tiRand z lo hi trig = mkFilterId z "TIRand" [lo, hi, trig] 1

-- | Random value in uniform distribution on trigger.
tRand :: UGenId -> UGen -> UGen -> UGen -> UGen
tRand z lo hi trig = mkFilterId z "TRand" [lo, hi, trig] 1

-- | Triggered windex.
twindex :: UGenId -> UGen -> UGen -> UGen -> UGen
twindex z i n a = mkFilterMCEId z "TWindex" [i, n] a 1

-- | White noise.
whiteNoise :: UGenId -> Rate -> UGen
whiteNoise z r = mkOscId z r "WhiteNoise" [] 1
