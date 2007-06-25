module Sound.SC3.UGen.Noise.Base where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, UGenID, mkOsc, mkFilter)

-- | Brown noise.
brownNoise :: Rate -> (UGenID -> UGen)
brownNoise r = mkOsc r "BrownNoise" [] 1

-- | Clip noise.
clipNoise :: Rate -> (UGenID -> UGen)
clipNoise r = mkOsc r "ClipNoise" [] 1

-- | Randomly pass or block triggers.
coinGate :: UGen -> UGen -> (UGenID -> UGen)
coinGate prob i = mkFilter "CoinGate" [prob,i] 1

-- | Random impulses in (-1, 1).
dust2 :: Rate -> UGen -> (UGenID -> UGen)
dust2 r density = mkOsc r "Dust2" [density] 1

-- | Random impulse in (0,1).
dust :: Rate -> UGen -> (UGenID -> UGen)
dust r density = mkOsc r "Dust" [density] 1

-- | Random value in exponential distribution.
expRand :: UGen -> UGen -> (UGenID -> UGen)
expRand lo hi = mkOsc IR "ExpRand" [lo,hi] 1

-- | Gray noise.
grayNoise :: Rate -> (UGenID -> UGen)
grayNoise r = mkOsc r "GrayNoise" [] 1

-- | Random integer in uniform distribution.
iRand :: UGen -> UGen -> (UGenID -> UGen)
iRand lo hi = mkOsc IR "IRand" [lo,hi] 1

-- | Clip noise.
lfClipNoise :: Rate -> UGen -> (UGenID -> UGen)
lfClipNoise r freq = mkOsc r "LFClipNoise" [freq] 1

-- | Dynamic clip noise.
lfdClipNoise :: Rate -> UGen -> (UGenID -> UGen)
lfdClipNoise r freq = mkOsc r "LFDClipNoise" [freq] 1

-- | Dynamic step noise.
lfdNoise0 :: Rate -> UGen -> (UGenID -> UGen)
lfdNoise0 r freq = mkOsc r "LFDNoise0" [freq] 1

-- | Dynamic ramp noise. 
lfdNoise1 :: Rate -> UGen -> (UGenID -> UGen)
lfdNoise1 r freq = mkOsc r "LFDNoise1" [freq] 1

-- | Dynamic quadratic noise
lfdNoise2 :: Rate -> UGen -> (UGenID -> UGen)
lfdNoise2 r freq = mkOsc r "LFDNoise2" [freq] 1

-- | Step noise.
lfNoise0 :: Rate -> UGen -> (UGenID -> UGen)
lfNoise0 r freq = mkOsc r "LFNoise0" [freq] 1

-- | Ramp noise.
lfNoise1 :: Rate -> UGen -> (UGenID -> UGen)
lfNoise1 r freq = mkOsc r "LFNoise1" [freq] 1

-- | Quadratic noise.
lfNoise2 :: Rate -> UGen -> (UGenID -> UGen)
lfNoise2 r freq = mkOsc r "LFNoise2" [freq] 1

-- | Random value in skewed linear distribution.
linRand :: UGen -> UGen -> UGen -> (UGenID -> UGen)
linRand lo hi m = mkOsc IR "LinRand" [lo,hi,m] 1

-- | Random value in sum of n linear distribution.
nRand :: UGen -> UGen -> UGen -> (UGenID -> UGen)
nRand lo hi n = mkOsc IR "NRand" [lo,hi,n] 1

-- | Pink noise.
pinkNoise :: Rate -> (UGenID -> UGen)
pinkNoise r = mkOsc r "PinkNoise" [] 1

-- | Random value in uniform distribution.
rand :: UGen -> UGen -> (UGenID -> UGen)
rand lo hi = mkOsc IR "Rand" [lo,hi] 1

-- | Random value in exponential distribution on trigger.
tExpRand :: UGen -> UGen -> UGen -> (UGenID -> UGen)
tExpRand lo hi trig = mkFilter "TExpRand" [lo,hi,trig] 1

-- | Random integer in uniform distribution on trigger.
tiRand :: UGen -> UGen -> UGen -> (UGenID -> UGen)
tiRand lo hi trig = mkFilter "TIRand" [lo,hi,trig] 1

-- | Random value in uniform distribution on trigger.
tRand :: UGen -> UGen -> UGen -> (UGenID -> UGen)
tRand lo hi trig = mkFilter "TRand" [lo,hi,trig] 1

-- | White noise.
whiteNoise :: Rate -> (UGenID -> UGen)
whiteNoise r = mkOsc r "WhiteNoise" [] 1
