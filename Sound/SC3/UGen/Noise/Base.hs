module Sound.SC3.UGen.Noise.Base where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, mkOsc, mkFilter)

-- | Brown noise.
brownNoise :: Rate -> UGen
brownNoise r = mkOsc r "BrownNoise" [] 1 0

-- | Clip noise.
clipNoise :: Rate -> UGen
clipNoise r = mkOsc r "ClipNoise" [] 1 0

-- | Randomly pass or block triggers.
coinGate :: UGen -> UGen -> UGen
coinGate prob i = mkFilter "CoinGate" [prob,i] 1 0

-- | Random impulses in (-1, 1).
dust2 :: Rate -> UGen -> UGen
dust2 r density = mkOsc r "Dust2" [density] 1 0

-- | Random impulse in (0,1).
dust :: Rate -> UGen -> UGen
dust r density = mkOsc r "Dust" [density] 1 0

-- | Random value in exponential distribution.
expRand :: UGen -> UGen -> UGen
expRand lo hi = mkOsc IR "ExpRand" [lo,hi] 1 0

-- | Gray noise.
grayNoise :: Rate -> UGen
grayNoise r = mkOsc r "GrayNoise" [] 1 0

-- | Random integer in uniform distribution.
iRand :: UGen -> UGen -> UGen
iRand lo hi = mkOsc IR "IRand" [lo,hi] 1 0

-- | Clip noise.
lfClipNoise :: Rate -> UGen -> UGen
lfClipNoise r freq = mkOsc r "LFClipNoise" [freq] 1 0

-- | Dynamic clip noise.
lfdClipNoise :: Rate -> UGen -> UGen
lfdClipNoise r freq = mkOsc r "LFDClipNoise" [freq] 1 0

-- | Dynamic step noise.
lfdNoise0 :: Rate -> UGen -> UGen
lfdNoise0 r freq = mkOsc r "LFDNoise0" [freq] 1 0

-- | Dynamic ramp noise. 
lfdNoise1 :: Rate -> UGen -> UGen
lfdNoise1 r freq = mkOsc r "LFDNoise1" [freq] 1 0

-- | Dynamic quadratic noise
lfdNoise2 :: Rate -> UGen -> UGen
lfdNoise2 r freq = mkOsc r "LFDNoise2" [freq] 1 0

-- | Step noise.
lfNoise0 :: Rate -> UGen -> UGen
lfNoise0 r freq = mkOsc r "LFNoise0" [freq] 1 0

-- | Ramp noise.
lfNoise1 :: Rate -> UGen -> UGen
lfNoise1 r freq = mkOsc r "LFNoise1" [freq] 1 0

-- | Quadratic noise.
lfNoise2 :: Rate -> UGen -> UGen
lfNoise2 r freq = mkOsc r "LFNoise2" [freq] 1 0

-- | Random value in skewed linear distribution.
linRand :: UGen -> UGen -> UGen -> UGen
linRand lo hi m = mkOsc IR "LinRand" [lo,hi,m] 1 0

-- | Random value in sum of n linear distribution.
nRand :: UGen -> UGen -> UGen -> UGen
nRand lo hi n = mkOsc IR "NRand" [lo,hi,n] 1 0

-- | Pink noise.
pinkNoise :: Rate -> UGen
pinkNoise r = mkOsc r "PinkNoise" [] 1 0

-- | Random value in uniform distribution.
rand :: UGen -> UGen -> UGen
rand lo hi = mkOsc IR "Rand" [lo,hi] 1 0

-- | Random value in exponential distribution on trigger.
tExpRand :: UGen -> UGen -> UGen -> UGen
tExpRand lo hi trig = mkFilter "TExpRand" [lo,hi,trig] 1 0

-- | Random integer in uniform distribution on trigger.
tiRand :: UGen -> UGen -> UGen -> UGen
tiRand lo hi trig = mkFilter "TIRand" [lo,hi,trig] 1 0

-- | Random value in uniform distribution on trigger.
tRand :: UGen -> UGen -> UGen -> UGen
tRand lo hi trig = mkFilter "TRand" [lo,hi,trig] 1 0

-- | White noise.
whiteNoise :: Rate -> UGen
whiteNoise r = mkOsc r "WhiteNoise" [] 1 0
