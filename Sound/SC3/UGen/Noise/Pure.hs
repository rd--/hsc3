module Sound.SC3.UGen.Noise.Pure where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, UId,
                            mkOscUId, mkFilterUId,)

-- | Brown noise.
brownNoise :: UId -> Rate -> UGen
brownNoise uid r = mkOscUId uid r "BrownNoise" [] 1 0

-- | Clip noise.
clipNoise :: UId -> Rate -> UGen
clipNoise uid r = mkOscUId uid r "ClipNoise" [] 1 0

-- | Randomly pass or block triggers.
coinGate :: UId -> UGen -> UGen -> UGen
coinGate uid prob i = mkFilterUId uid "CoinGate" [prob,i] 1 0

-- | Random impulses in (-1, 1).
dust2 :: UId -> Rate -> UGen -> UGen
dust2 uid r density = mkOscUId uid r "Dust2" [density] 1 0

-- | Random impulse in (0,1).
dust :: UId -> Rate -> UGen -> UGen
dust uid r density = mkOscUId uid r "Dust" [density] 1 0

-- | Random value in exponential distribution.
expRand :: UId -> UGen -> UGen -> UGen
expRand uid lo hi = mkOscUId uid IR "ExpRand" [lo,hi] 1 0

-- | Gray noise.
grayNoise :: UId -> Rate -> UGen
grayNoise uid r = mkOscUId uid r "GrayNoise" [] 1 0

-- | Random integer in uniform distribution.
iRand :: UId -> UGen -> UGen -> UGen
iRand uid lo hi = mkOscUId uid IR "IRand" [lo,hi] 1 0

-- | Clip noise.
lfClipNoise :: UId -> Rate -> UGen -> UGen
lfClipNoise uid r freq = mkOscUId uid r "LFClipNoise" [freq] 1 0

-- | Dynamic clip noise.
lfdClipNoise :: UId -> Rate -> UGen -> UGen
lfdClipNoise uid r freq = mkOscUId uid r "LFDClipNoise" [freq] 1 0

-- | Dynamic step noise.
lfdNoise0 :: UId -> Rate -> UGen -> UGen
lfdNoise0 uid r freq = mkOscUId uid r "LFDNoise0" [freq] 1 0

-- | Dynamic ramp noise. 
lfdNoise1 :: UId -> Rate -> UGen -> UGen
lfdNoise1 uid r freq = mkOscUId uid r "LFDNoise1" [freq] 1 0

-- | Dynamic quadratic noise
lfdNoise2 :: UId -> Rate -> UGen -> UGen
lfdNoise2 uid r freq = mkOscUId uid r "LFDNoise2" [freq] 1 0

-- | Step noise.
lfNoise0 :: UId -> Rate -> UGen -> UGen
lfNoise0 uid r freq = mkOscUId uid r "LFNoise0" [freq] 1 0

-- | Ramp noise.
lfNoise1 :: UId -> Rate -> UGen -> UGen
lfNoise1 uid r freq = mkOscUId uid r "LFNoise1" [freq] 1 0

-- | Quadratic noise.
lfNoise2 :: UId -> Rate -> UGen -> UGen
lfNoise2 uid r freq = mkOscUId uid r "LFNoise2" [freq] 1 0

-- | Random value in skewed linear distribution.
linRand :: UId -> UGen -> UGen -> UGen -> UGen
linRand uid lo hi m = mkOscUId uid IR "LinRand" [lo,hi,m] 1 0

-- | Random value in sum of n linear distribution.
nRand :: UId -> UGen -> UGen -> UGen -> UGen
nRand uid lo hi n = mkOscUId uid IR "NRand" [lo,hi,n] 1 0

-- | Pink noise.
pinkNoise :: UId -> Rate -> UGen
pinkNoise uid r = mkOscUId uid r "PinkNoise" [] 1 0

-- | Random value in uniform distribution.
rand :: UId -> UGen -> UGen -> UGen
rand uid lo hi = mkOscUId uid IR "Rand" [lo,hi] 1 0

-- | Random value in exponential distribution on trigger.
tExpRand :: UId -> UGen -> UGen -> UGen -> UGen
tExpRand uid lo hi trig = mkFilterUId uid "TExpRand" [lo,hi,trig] 1 0

-- | Random integer in uniform distribution on trigger.
tiRand :: UId -> UGen -> UGen -> UGen -> UGen
tiRand uid lo hi trig = mkFilterUId uid "TIRand" [lo,hi,trig] 1 0

-- | Random value in uniform distribution on trigger.
tRand :: UId -> UGen -> UGen -> UGen -> UGen
tRand uid lo hi trig = mkFilterUId uid "TRand" [lo,hi,trig] 1 0

-- | White noise.
whiteNoise :: UId -> Rate -> UGen
whiteNoise uid r = mkOscUId uid r "WhiteNoise" [] 1 0
