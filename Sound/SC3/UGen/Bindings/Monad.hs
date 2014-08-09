-- | Monad constructors for 'UGen's.
module Sound.SC3.UGen.Bindings.Monad where

import Sound.SC3.UGen.Bindings.DB
import Sound.SC3.UGen.Bindings.HW
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UId

-- * Demand

-- | Buffer demand ugen.
dbufrdM :: (UId m) => UGen -> UGen -> Loop -> m UGen
dbufrdM = liftUId3 dbufrd

-- | Buffer write on demand unit generator.
dbufwrM :: (UId m) => UGen -> UGen -> UGen -> Loop -> m UGen
dbufwrM = liftUId4 dbufwr

-- | Demand rate white noise.
dwhiteM :: (UId m) => UGen -> UGen -> UGen -> m UGen
dwhiteM = liftUId3 dwhite

-- | Demand rate integer white noise.
diwhiteM :: (UId m) => UGen -> UGen -> UGen -> m UGen
diwhiteM = liftUId3 diwhite

-- | Demand rate brown noise.
dbrownM :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
dbrownM = liftUId4 dbrown

-- | Demand rate integer brown noise.
dibrownM :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
dibrownM = liftUId4 dibrown

-- | Demand rate random selection.
drandM :: (UId m) => UGen -> UGen -> m UGen
drandM = liftUId2 drand

-- | Demand rate weighted random sequence generator.
dwrandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
dwrandM = liftUId3 dwrand

-- | Demand rate random selection with no immediate repetition.
dxrandM :: (UId m) => UGen -> UGen -> m UGen
dxrandM = liftUId2 dxrand

-- | Demand rate arithmetic series.
dseriesM :: (UId m) => UGen -> UGen -> UGen -> m UGen
dseriesM = liftUId3 dseries

-- | Demand rate geometric series.
dgeomM :: (UId m) => UGen -> UGen -> UGen -> m UGen
dgeomM = liftUId3 dgeom

-- | Demand rate sequence generator.
dseqM :: (UId m) => UGen -> UGen -> m UGen
dseqM = liftUId2 dseq

-- | Demand rate series generator.
dserM :: (UId m) => UGen -> UGen -> m UGen
dserM = liftUId2 dser

-- | Demand rate sequence shuffler.
dshufM :: (UId m) => UGen -> UGen -> m UGen
dshufM = liftUId2 dshuf

-- | Demand input replication
dstutterM :: (UId m) => UGen -> UGen -> m UGen
dstutterM = liftUId2 dstutter

-- | Demand rate input switching.
dswitch1M :: (UId m) => UGen -> UGen -> m UGen
dswitch1M = liftUId2 dswitch1

-- | Demand rate input switching.
dswitchM :: (UId m) => UGen -> UGen -> m UGen
dswitchM = liftUId2 dswitch

-- * FFT

-- | Randomize order of bins.
pv_BinScrambleM :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
pv_BinScrambleM = liftUId4 pv_BinScramble

-- | Randomly clear bins.
pv_RandCombM :: (UId m) => UGen -> UGen -> UGen -> m UGen
pv_RandCombM = liftUId3 pv_RandComb

-- | Cross fade, copying bins in random order.
pv_RandWipeM :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
pv_RandWipeM = liftUId4 pv_RandWipe

-- * Noise

-- | Brown noise.
brownNoiseM :: (UId m) => Rate -> m UGen
brownNoiseM = liftUId brownNoise

-- | Clip noise.
clipNoiseM :: (UId m) => Rate -> m UGen
clipNoiseM = liftUId clipNoise

-- | Randomly pass or block triggers.
coinGateM :: (UId m) => UGen -> UGen -> m UGen
coinGateM = liftUId2 coinGate

-- | Random impulses in (-1, 1).
dust2M :: (UId m) => Rate -> UGen -> m UGen
dust2M = liftUId2 dust2

-- | Random impulse in (0,1).
dustM :: (UId m) => Rate -> UGen -> m UGen
dustM = liftUId2 dust

-- | Random value in exponential distribution.
expRandM :: (UId m) => UGen -> UGen -> m UGen
expRandM = liftUId2 expRand

-- | Gray noise.
grayNoiseM :: (UId m) => Rate -> m UGen
grayNoiseM = liftUId grayNoise

-- | Random integer in uniform distribution.
iRandM :: (UId m) => UGen -> UGen -> m UGen
iRandM = liftUId2 iRand

-- | Clip noise.
lfClipNoiseM :: (UId m) => Rate -> UGen -> m UGen
lfClipNoiseM = liftUId2 lfClipNoise

-- | Dynamic clip noise.
lfdClipNoiseM :: (UId m) => Rate -> UGen -> m UGen
lfdClipNoiseM = liftUId2 lfdClipNoise

-- | Dynamic step noise.
lfdNoise0M :: (UId m) => Rate -> UGen -> m UGen
lfdNoise0M = liftUId2 lfdNoise0

-- | Dynamic ramp noise.
lfdNoise1M :: (UId m) => Rate -> UGen -> m UGen
lfdNoise1M = liftUId2 lfdNoise1

-- | Dynamic cubic noise
lfdNoise3M :: (UId m) => Rate -> UGen -> m UGen
lfdNoise3M = liftUId2 lfdNoise3

-- | Step noise.
lfNoise0M :: (UId m) => Rate -> UGen -> m UGen
lfNoise0M = liftUId2 lfNoise0

-- | Ramp noise.
lfNoise1M :: (UId m) => Rate -> UGen -> m UGen
lfNoise1M = liftUId2 lfNoise1

-- | Quadratic noise.
lfNoise2M :: (UId m) => Rate -> UGen -> m UGen
lfNoise2M = liftUId2 lfNoise2

-- | Random value in skewed linear distribution.
linRandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
linRandM = liftUId3 linRand

-- | Random value in sum of n linear distribution.
nRandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
nRandM = liftUId3 nRand

-- | Pink noise.
pinkNoiseM :: (UId m) => Rate -> m UGen
pinkNoiseM = liftUId pinkNoise

-- | Random value in uniform distribution.
randM :: (UId m) => UGen -> UGen -> m UGen
randM = liftUId2 rand

-- | Random value in exponential distribution on trigger.
tExpRandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
tExpRandM = liftUId3 tExpRand

-- | Random integer in uniform distribution on trigger.
tIRandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
tIRandM = liftUId3 tIRand

-- | Random value in uniform distribution on trigger.
tRandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
tRandM = liftUId3 tRand

-- | Triggered windex.
tWindexM :: (UId m) => UGen -> UGen -> UGen -> m UGen
tWindexM = liftUId3 tWindex

-- | White noise.
whiteNoiseM :: (UId m) => Rate -> m UGen
whiteNoiseM = liftUId whiteNoise
