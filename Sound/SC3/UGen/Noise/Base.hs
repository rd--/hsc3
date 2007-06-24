module Sound.SC3.UGen.Noise.Base where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, UGenGen, mkOsc, mkFilter)

-- | Brown noise.
brownNoise :: Rate -> UGenGen
brownNoise r = mkOsc r "BrownNoise" [] 1

-- | Clip noise.
clipNoise :: Rate -> UGenGen
clipNoise r = mkOsc r "ClipNoise" [] 1

-- | Randomly pass or block triggers.
coinGate :: UGen -> UGen -> UGenGen
coinGate prob i = mkFilter "CoinGate" [prob,i] 1

-- | Random impulses in (-1, 1).
dust2 :: Rate -> UGen -> UGenGen
dust2 r density = mkOsc r "Dust2" [density] 1

-- | Random impulse in (0,1).
dust :: Rate -> UGen -> UGenGen
dust r density = mkOsc r "Dust" [density] 1

-- | Random value in exponential distribution.
expRand :: UGen -> UGen -> UGenGen
expRand lo hi = mkOsc IR "ExpRand" [lo,hi] 1

-- | Gray noise.
grayNoise :: Rate -> UGenGen
grayNoise r = mkOsc r "GrayNoise" [] 1

-- | Random integer in uniform distribution.
iRand :: UGen -> UGen -> UGenGen
iRand lo hi = mkOsc IR "IRand" [lo,hi] 1

-- | Clip noise.
lfClipNoise :: Rate -> UGen -> UGenGen
lfClipNoise r freq = mkOsc r "LFClipNoise" [freq] 1

-- | Dynamic clip noise.
lfdClipNoise :: Rate -> UGen -> UGenGen
lfdClipNoise r freq = mkOsc r "LFDClipNoise" [freq] 1

-- | Dynamic step noise.
lfdNoise0 :: Rate -> UGen -> UGenGen
lfdNoise0 r freq = mkOsc r "LFDNoise0" [freq] 1

-- | Dynamic ramp noise. 
lfdNoise1 :: Rate -> UGen -> UGenGen
lfdNoise1 r freq = mkOsc r "LFDNoise1" [freq] 1

-- | Dynamic quadratic noise
lfdNoise2 :: Rate -> UGen -> UGenGen
lfdNoise2 r freq = mkOsc r "LFDNoise2" [freq] 1

-- | Step noise.
lfNoise0 :: Rate -> UGen -> UGenGen
lfNoise0 r freq = mkOsc r "LFNoise0" [freq] 1

-- | Ramp noise.
lfNoise1 :: Rate -> UGen -> UGenGen
lfNoise1 r freq = mkOsc r "LFNoise1" [freq] 1

-- | Quadratic noise.
lfNoise2 :: Rate -> UGen -> UGenGen
lfNoise2 r freq = mkOsc r "LFNoise2" [freq] 1

-- | Random value in skewed linear distribution.
linRand :: UGen -> UGen -> UGen -> UGenGen
linRand lo hi m = mkOsc IR "LinRand" [lo,hi,m] 1

-- | Random value in sum of n linear distribution.
nRand :: UGen -> UGen -> UGen -> UGenGen
nRand lo hi n = mkOsc IR "NRand" [lo,hi,n] 1

-- | Pink noise.
pinkNoise :: Rate -> UGenGen
pinkNoise r = mkOsc r "PinkNoise" [] 1

-- | Random value in uniform distribution.
rand :: UGen -> UGen -> UGenGen
rand lo hi = mkOsc IR "Rand" [lo,hi] 1

-- | Random value in exponential distribution on trigger.
tExpRand :: UGen -> UGen -> UGen -> UGenGen
tExpRand lo hi trig = mkFilter "TExpRand" [lo,hi,trig] 1

-- | Random integer in uniform distribution on trigger.
tiRand :: UGen -> UGen -> UGen -> UGenGen
tiRand lo hi trig = mkFilter "TIRand" [lo,hi,trig] 1

-- | Random value in uniform distribution on trigger.
tRand :: UGen -> UGen -> UGen -> UGenGen
tRand lo hi trig = mkFilter "TRand" [lo,hi,trig] 1

-- | White noise.
whiteNoise :: Rate -> UGenGen
whiteNoise r = mkOsc r "WhiteNoise" [] 1
