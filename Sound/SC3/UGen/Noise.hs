module Sound.SC3.UGen.Noise where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, UId,
                            mkOscUId,
                            mkFilterUId,
                            uniquify, zeroUId)

-- | Brown noise.
brownNoise :: Rate -> IO UGen
brownNoise r = uniquify (brownNoiseUId zeroUId r)

brownNoiseUId :: UId -> Rate -> UGen
brownNoiseUId uid r = mkOscUId uid r "BrownNoise" [] 1 0

-- | Clip noise.
clipNoise :: Rate -> IO UGen
clipNoise r = uniquify (clipNoiseUId zeroUId r)

clipNoiseUId :: UId -> Rate -> UGen
clipNoiseUId uid r = mkOscUId uid r "ClipNoise" [] 1 0

-- | Randomly pass or block triggers.
coinGate :: UGen -> UGen -> IO UGen
coinGate prob i = uniquify (coinGateUId zeroUId prob i)

coinGateUId :: UId -> UGen -> UGen -> UGen
coinGateUId uid prob i = mkFilterUId uid "CoinGate" [prob,i] 1 0

-- | Random impulses in (-1, 1).
dust2 :: Rate -> UGen -> IO UGen
dust2 r density = uniquify (dust2UId zeroUId r density)

dust2UId :: UId -> Rate -> UGen -> UGen
dust2UId uid r density = mkOscUId uid r "Dust2" [density] 1 0

-- | Random impulse in (0,1).
dust :: Rate -> UGen -> IO UGen
dust r density = uniquify (dustUId zeroUId r density)

dustUId :: UId -> Rate -> UGen -> UGen
dustUId uid r density = mkOscUId uid r "Dust" [density] 1 0

-- | Random value in exponential distribution.
expRand :: UGen -> UGen -> IO UGen
expRand lo hi = uniquify (expRandUId zeroUId lo hi)

expRandUId :: UId -> UGen -> UGen -> UGen
expRandUId uid lo hi = mkOscUId uid IR "ExpRand" [lo,hi] 1 0

-- | Gray noise.
grayNoise :: Rate -> IO UGen
grayNoise r = uniquify (grayNoiseUId zeroUId r)

grayNoiseUId :: UId -> Rate -> UGen
grayNoiseUId uid r = mkOscUId uid r "GrayNoise" [] 1 0

-- | Random integer in uniform distribution.
iRand :: UGen -> UGen -> IO UGen
iRand lo hi = uniquify (iRandUId zeroUId lo hi)

iRandUId :: UId -> UGen -> UGen -> UGen
iRandUId uid lo hi = mkOscUId uid IR "IRand" [lo,hi] 1 0

-- | Clip noise.
lfClipNoise :: Rate -> UGen -> IO UGen
lfClipNoise r freq = uniquify (lfClipNoiseUId zeroUId r freq)

lfClipNoiseUId :: UId -> Rate -> UGen -> UGen
lfClipNoiseUId uid r freq = mkOscUId uid r "LFClipNoise" [freq] 1 0

-- | Dynamic clip noise.
lfdClipNoise :: Rate -> UGen -> IO UGen
lfdClipNoise r freq = uniquify (lfdClipNoiseUId zeroUId r freq)

lfdClipNoiseUId :: UId -> Rate -> UGen -> UGen
lfdClipNoiseUId uid r freq = mkOscUId uid r "LFDClipNoise" [freq] 1 0

-- | Dynamic step noise.
lfdNoise0 :: Rate -> UGen -> IO UGen
lfdNoise0 r freq = uniquify (lfdNoise0UId zeroUId r freq)

lfdNoise0UId :: UId -> Rate -> UGen -> UGen
lfdNoise0UId uid r freq = mkOscUId uid r "LFDNoise0" [freq] 1 0

-- | Dynamic ramp noise. 
lfdNoise1 :: Rate -> UGen -> IO UGen
lfdNoise1 r freq = uniquify (lfdNoise1UId zeroUId r freq)

lfdNoise1UId :: UId -> Rate -> UGen -> UGen
lfdNoise1UId uid r freq = mkOscUId uid r "LFDNoise1" [freq] 1 0

-- | Dynamic quadratic noise
lfdNoise2 :: Rate -> UGen -> IO UGen
lfdNoise2 r freq = uniquify (lfdNoise2UId zeroUId r freq)

lfdNoise2UId :: UId -> Rate -> UGen -> UGen
lfdNoise2UId uid r freq = mkOscUId uid r "LFDNoise2" [freq] 1 0

-- | Step noise.
lfNoise0 :: Rate -> UGen -> IO UGen
lfNoise0 r freq = uniquify (lfNoise0UId zeroUId r freq)

lfNoise0UId :: UId -> Rate -> UGen -> UGen
lfNoise0UId uid r freq = mkOscUId uid r "LFNoise0" [freq] 1 0

-- | Ramp noise.
lfNoise1 :: Rate -> UGen -> IO UGen
lfNoise1 r freq = uniquify (lfNoise1UId zeroUId r freq)

lfNoise1UId :: UId -> Rate -> UGen -> UGen
lfNoise1UId uid r freq = mkOscUId uid r "LFNoise1" [freq] 1 0

-- | Quadratic noise.
lfNoise2 :: Rate -> UGen -> IO UGen
lfNoise2 r freq = uniquify (lfNoise2UId zeroUId r freq)

lfNoise2UId :: UId -> Rate -> UGen -> UGen
lfNoise2UId uid r freq = mkOscUId uid r "LFNoise2" [freq] 1 0

-- | Random value in skewed linear distribution.
linRand :: UGen -> UGen -> UGen -> IO UGen
linRand lo hi m = uniquify (linRandUId zeroUId lo hi m)

linRandUId :: UId -> UGen -> UGen -> UGen -> UGen
linRandUId uid lo hi m = mkOscUId uid IR "LinRand" [lo,hi,m] 1 0

-- | Random value in sum of n linear distribution.
nRand :: UGen -> UGen -> UGen -> IO UGen
nRand lo hi n = uniquify (nRandUId zeroUId lo hi n)

nRandUId :: UId -> UGen -> UGen -> UGen -> UGen
nRandUId uid lo hi n = mkOscUId uid IR "NRand" [lo,hi,n] 1 0

-- | Pink noise.
pinkNoise :: Rate -> IO UGen
pinkNoise r = uniquify (pinkNoiseUId zeroUId r)

pinkNoiseUId :: UId -> Rate -> UGen
pinkNoiseUId uid r = mkOscUId uid r "PinkNoise" [] 1 0

-- | Random value in uniform distribution.
rand :: UGen -> UGen -> IO UGen
rand lo hi = uniquify (randUId zeroUId lo hi)

randUId :: UId -> UGen -> UGen -> UGen
randUId uid lo hi = mkOscUId uid IR "Rand" [lo,hi] 1 0

-- | Random value in exponential distribution on trigger.
tExpRand :: UGen -> UGen -> UGen -> IO UGen
tExpRand lo hi trig = uniquify (tExpRandUId zeroUId lo hi trig)

tExpRandUId :: UId -> UGen -> UGen -> UGen -> UGen
tExpRandUId uid lo hi trig = mkFilterUId uid "TExpRand" [lo,hi,trig] 1 0

-- | Random integer in uniform distribution on trigger.
tiRand :: UGen -> UGen -> UGen -> IO UGen
tiRand lo hi trig = uniquify (tiRandUId zeroUId lo hi trig)

tiRandUId :: UId -> UGen -> UGen -> UGen -> UGen
tiRandUId uid lo hi trig = mkFilterUId uid "TIRand" [lo,hi,trig] 1 0

-- | Random value in uniform distribution on trigger.
tRand :: UGen -> UGen -> UGen -> IO UGen
tRand lo hi trig = uniquify (tRandUId zeroUId lo hi trig)

tRandUId :: UId -> UGen -> UGen -> UGen -> UGen
tRandUId uid lo hi trig = mkFilterUId uid "TRand" [lo,hi,trig] 1 0

-- | White noise.
whiteNoise :: Rate -> IO UGen
whiteNoise r = uniquify (whiteNoiseUId zeroUId r)

whiteNoiseUId :: UId -> Rate -> UGen
whiteNoiseUId uid r = mkOscUId uid r "WhiteNoise" [] 1 0
