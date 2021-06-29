-- | Monad constructors for 'UGen's.
module Sound.SC3.UGen.Bindings.Monad where

import Control.Monad {- base -}

import Sound.SC3.Common.Enum
import Sound.SC3.Common.Rate
import Sound.SC3.Common.UId
import Sound.SC3.UGen.Bindings.DB
import Sound.SC3.UGen.Bindings.HW
import Sound.SC3.UGen.Type

-- | Clone a unit generator (mce . replicateM).
clone :: UId m => Int -> m UGen -> m UGen
clone n = fmap mce . replicateM n

-- * Demand

-- | 'dbufrd'
dbufrdM :: UId m => UGen -> UGen -> Loop UGen -> m UGen
dbufrdM = liftUId3 dbufrdId

-- | 'dbufwr'
dbufwrM :: UId m => UGen -> UGen -> UGen -> Loop UGen -> m UGen
dbufwrM = liftUId4 dbufwrId

-- | 'dconst'
dconstM :: UId m => UGen -> UGen -> UGen -> m UGen
dconstM = liftUId3 dconstId

-- | 'dwhite'
dwhiteM :: UId m => UGen -> UGen -> UGen -> m UGen
dwhiteM = liftUId3 dwhiteId

-- | 'diwhite'
diwhiteM :: UId m => UGen -> UGen -> UGen -> m UGen
diwhiteM = liftUId3 diwhiteId

-- | 'dbrown'
dbrownM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
dbrownM = liftUId4 dbrownId

-- | 'dibrown'
dibrownM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
dibrownM = liftUId4 dibrownId

-- | 'dpoll'
dpollM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
dpollM = liftUId4 dpollId

-- | 'drand'
drandM :: UId m => UGen -> UGen -> m UGen
drandM = liftUId2 drandId

-- | 'dreset'
dresetM :: UId m => UGen -> UGen -> m UGen
dresetM = liftUId2 dresetId

-- | 'dunique'
duniqueM :: UId m => UGen -> UGen -> UGen -> m UGen
duniqueM = liftUId3 duniqueId

-- | 'dwrand'
dwrandM :: UId m => UGen -> UGen -> UGen -> m UGen
dwrandM = liftUId3 dwrandId

-- | 'dxrand'
dxrandM :: UId m => UGen -> UGen -> m UGen
dxrandM = liftUId2 dxrandId

-- | Demand rate arithmetic series.
dseriesM :: UId m => UGen -> UGen -> UGen -> m UGen
dseriesM = liftUId3 dseriesId

-- | Demand rate geometric series.
dgeomM :: UId m => UGen -> UGen -> UGen -> m UGen
dgeomM = liftUId3 dgeomId

-- | Demand rate sequence generator.
dseqM :: UId m => UGen -> UGen -> m UGen
dseqM = liftUId2 dseqId

-- | Demand rate series generator.
dserM :: UId m => UGen -> UGen -> m UGen
dserM = liftUId2 dserId

-- | Demand rate sequence shuffler.
dshufM :: UId m => UGen -> UGen -> m UGen
dshufM = liftUId2 dshufId

-- | Demand input replication
dstutterM :: UId m => UGen -> UGen -> m UGen
dstutterM = liftUId2 dstutterId

-- | Demand rate input switching.
dswitch1M :: UId m => UGen -> UGen -> m UGen
dswitch1M = liftUId2 dswitch1Id

-- | Demand rate input switching.
dswitchM :: UId m => UGen -> UGen -> m UGen
dswitchM = liftUId2 dswitchId

-- * FFT

-- | Randomize order of bins.
pv_BinScrambleM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
pv_BinScrambleM = liftUId4 pv_BinScrambleId

-- | Randomly clear bins.
pv_RandCombM :: UId m => UGen -> UGen -> UGen -> m UGen
pv_RandCombM = liftUId3 pv_RandCombId

-- | Cross fade, copying bins in random order.
pv_RandWipeM :: UId m => UGen -> UGen -> UGen -> UGen -> m UGen
pv_RandWipeM = liftUId4 pv_RandWipeId

-- * Noise

-- | Brown noise.
brownNoiseM :: UId m => Rate -> m UGen
brownNoiseM = liftUId1 brownNoiseId

-- | Clip noise.
clipNoiseM :: UId m => Rate -> m UGen
clipNoiseM = liftUId1 clipNoiseId

-- | Randomly pass or block triggers.
coinGateM :: UId m => UGen -> UGen -> m UGen
coinGateM = liftUId2 coinGateId

-- | Random impulses in (-1, 1).
dust2M :: UId m => Rate -> UGen -> m UGen
dust2M = liftUId2 dust2Id

-- | Random impulse in (0,1).
dustM :: UId m => Rate -> UGen -> m UGen
dustM = liftUId2 dustId

-- | Random value in exponential distribution.
expRandM :: UId m => UGen -> UGen -> m UGen
expRandM = liftUId2 expRandId

-- | Gray noise.
grayNoiseM :: UId m => Rate -> m UGen
grayNoiseM = liftUId1 grayNoiseId

-- | Random integer in uniform distribution.
iRandM :: UId m => UGen -> UGen -> m UGen
iRandM = liftUId2 iRandId

-- | Clip noise.
lfClipNoiseM :: UId m => Rate -> UGen -> m UGen
lfClipNoiseM = liftUId2 lfClipNoiseId

-- | Dynamic clip noise.
lfdClipNoiseM :: UId m => Rate -> UGen -> m UGen
lfdClipNoiseM = liftUId2 lfdClipNoiseId

-- | Dynamic step noise.
lfdNoise0M :: UId m => Rate -> UGen -> m UGen
lfdNoise0M = liftUId2 lfdNoise0Id

-- | Dynamic ramp noise.
lfdNoise1M :: UId m => Rate -> UGen -> m UGen
lfdNoise1M = liftUId2 lfdNoise1Id

-- | Dynamic cubic noise
lfdNoise3M :: UId m => Rate -> UGen -> m UGen
lfdNoise3M = liftUId2 lfdNoise3Id

-- | Step noise.
lfNoise0M :: UId m => Rate -> UGen -> m UGen
lfNoise0M = liftUId2 lfNoise0Id

-- | Ramp noise.
lfNoise1M :: UId m => Rate -> UGen -> m UGen
lfNoise1M = liftUId2 lfNoise1Id

-- | Quadratic noise.
lfNoise2M :: UId m => Rate -> UGen -> m UGen
lfNoise2M = liftUId2 lfNoise2Id

-- | Random value in skewed linear distribution.
linRandM :: UId m => UGen -> UGen -> UGen -> m UGen
linRandM = liftUId3 linRandId

-- | 'localBuf'
localBufM :: UId m => UGen -> UGen -> m UGen
localBufM = liftUId2 localBufId

-- | Random value in sum of n linear distribution.
nRandM :: UId m => UGen -> UGen -> UGen -> m UGen
nRandM = liftUId3 nRandId

-- | Pink noise.
pinkNoiseM :: UId m => Rate -> m UGen
pinkNoiseM = liftUId1 pinkNoiseId

-- | Random value in uniform distribution.
randM :: UId m => UGen -> UGen -> m UGen
randM = liftUId2 randId

-- | Random value in exponential distribution on trigger.
tExpRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tExpRandM = liftUId3 tExpRandId

-- | Random integer in uniform distribution on trigger.
tiRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tiRandM = liftUId3 tiRandId

-- | Random value in uniform distribution on trigger.
tRandM :: UId m => UGen -> UGen -> UGen -> m UGen
tRandM = liftUId3 tRandId

-- | Triggered windex.
tWindexM :: UId m => UGen -> UGen -> UGen -> m UGen
tWindexM = liftUId3 tWindexId

-- | White noise.
whiteNoiseM :: UId m => Rate -> m UGen
whiteNoiseM = liftUId1 whiteNoiseId
