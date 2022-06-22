-- | Monad constructors for 'Ugen's.
module Sound.Sc3.Ugen.Bindings.Monad where

import Control.Monad {- base -}

import Sound.Sc3.Common.Enum
import Sound.Sc3.Common.Rate
import Sound.Sc3.Common.Uid
import Sound.Sc3.Ugen.Bindings.Db
import Sound.Sc3.Ugen.Bindings.Hw
import Sound.Sc3.Ugen.Type

-- | Clone a unit generator (mce . replicateM).
clone :: Uid m => Int -> m Ugen -> m Ugen
clone n = fmap mce . replicateM n

-- * Demand

-- | 'dbufrd'
dbufrdM :: Uid m => Ugen -> Ugen -> Loop Ugen -> m Ugen
dbufrdM = liftUid3 dbufrdId

-- | 'dbufwr'
dbufwrM :: Uid m => Ugen -> Ugen -> Ugen -> Loop Ugen -> m Ugen
dbufwrM = liftUid4 dbufwrId

-- | 'dconst'
dconstM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dconstM = liftUid3 dconstId

-- | 'dwhite'
dwhiteM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dwhiteM = liftUid3 dwhiteId

-- | 'diwhite'
diwhiteM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
diwhiteM = liftUid3 diwhiteId

-- | 'dbrown'
dbrownM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
dbrownM = liftUid4 dbrownId

-- | 'dibrown'
dibrownM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
dibrownM = liftUid4 dibrownId

-- | 'dpoll'
dpollM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
dpollM = liftUid4 dpollId

-- | 'drand'
drandM :: Uid m => Ugen -> Ugen -> m Ugen
drandM = liftUid2 drandId

-- | 'dreset'
dresetM :: Uid m => Ugen -> Ugen -> m Ugen
dresetM = liftUid2 dresetId

-- | 'dunique'
duniqueM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
duniqueM = liftUid3 duniqueId

-- | 'dwrand'
dwrandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dwrandM = liftUid3 dwrandId

-- | 'dxrand'
dxrandM :: Uid m => Ugen -> Ugen -> m Ugen
dxrandM = liftUid2 dxrandId

-- | Demand rate arithmetic series.
dseriesM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dseriesM = liftUid3 dseriesId

-- | Demand rate geometric series.
dgeomM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dgeomM = liftUid3 dgeomId

-- | Demand rate sequence generator.
dseqM :: Uid m => Ugen -> Ugen -> m Ugen
dseqM = liftUid2 dseqId

-- | Demand rate series generator.
dserM :: Uid m => Ugen -> Ugen -> m Ugen
dserM = liftUid2 dserId

-- | Demand rate sequence shuffler.
dshufM :: Uid m => Ugen -> Ugen -> m Ugen
dshufM = liftUid2 dshufId

-- | Demand input replication
dstutterM :: Uid m => Ugen -> Ugen -> m Ugen
dstutterM = liftUid2 dstutterId

-- | Demand rate input switching.
dswitch1M :: Uid m => Ugen -> Ugen -> m Ugen
dswitch1M = liftUid2 dswitch1Id

-- | Demand rate input switching.
dswitchM :: Uid m => Ugen -> Ugen -> m Ugen
dswitchM = liftUid2 dswitchId

-- * FFT

-- | Randomize order of bins.
pv_BinScrambleM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
pv_BinScrambleM = liftUid4 pv_BinScrambleId

-- | Randomly clear bins.
pv_RandCombM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
pv_RandCombM = liftUid3 pv_RandCombId

-- | Cross fade, copying bins in random order.
pv_RandWipeM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
pv_RandWipeM = liftUid4 pv_RandWipeId

-- * Noise

-- | Brown noise.
brownNoiseM :: Uid m => Rate -> m Ugen
brownNoiseM = liftUid1 brownNoiseId

-- | Clip noise.
clipNoiseM :: Uid m => Rate -> m Ugen
clipNoiseM = liftUid1 clipNoiseId

-- | Randomly pass or block triggers.
coinGateM :: Uid m => Ugen -> Ugen -> m Ugen
coinGateM = liftUid2 coinGateId

-- | Random impulses in (-1, 1).
dust2M :: Uid m => Rate -> Ugen -> m Ugen
dust2M = liftUid2 dust2Id

-- | Random impulse in (0,1).
dustM :: Uid m => Rate -> Ugen -> m Ugen
dustM = liftUid2 dustId

-- | Random value in exponential distribution.
expRandM :: Uid m => Ugen -> Ugen -> m Ugen
expRandM = liftUid2 expRandId

-- | Gray noise.
grayNoiseM :: Uid m => Rate -> m Ugen
grayNoiseM = liftUid1 grayNoiseId

-- | Random integer in uniform distribution.
iRandM :: Uid m => Ugen -> Ugen -> m Ugen
iRandM = liftUid2 iRandId

-- | Clip noise.
lfClipNoiseM :: Uid m => Rate -> Ugen -> m Ugen
lfClipNoiseM = liftUid2 lfClipNoiseId

-- | Dynamic clip noise.
lfdClipNoiseM :: Uid m => Rate -> Ugen -> m Ugen
lfdClipNoiseM = liftUid2 lfdClipNoiseId

-- | Dynamic step noise.
lfdNoise0M :: Uid m => Rate -> Ugen -> m Ugen
lfdNoise0M = liftUid2 lfdNoise0Id

-- | Dynamic ramp noise.
lfdNoise1M :: Uid m => Rate -> Ugen -> m Ugen
lfdNoise1M = liftUid2 lfdNoise1Id

-- | Dynamic cubic noise
lfdNoise3M :: Uid m => Rate -> Ugen -> m Ugen
lfdNoise3M = liftUid2 lfdNoise3Id

-- | Step noise.
lfNoise0M :: Uid m => Rate -> Ugen -> m Ugen
lfNoise0M = liftUid2 lfNoise0Id

-- | Ramp noise.
lfNoise1M :: Uid m => Rate -> Ugen -> m Ugen
lfNoise1M = liftUid2 lfNoise1Id

-- | Quadratic noise.
lfNoise2M :: Uid m => Rate -> Ugen -> m Ugen
lfNoise2M = liftUid2 lfNoise2Id

-- | Random value in skewed linear distribution.
linRandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
linRandM = liftUid3 linRandId

-- | 'localBuf'
localBufM :: Uid m => Ugen -> Ugen -> m Ugen
localBufM = liftUid2 localBufId

-- | Random value in sum of n linear distribution.
nRandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
nRandM = liftUid3 nRandId

-- | Pink noise.
pinkNoiseM :: Uid m => Rate -> m Ugen
pinkNoiseM = liftUid1 pinkNoiseId

-- | Random value in uniform distribution.
randM :: Uid m => Ugen -> Ugen -> m Ugen
randM = liftUid2 randId

-- | Random value in exponential distribution on trigger.
tExpRandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
tExpRandM = liftUid3 tExpRandId

-- | Random integer in uniform distribution on trigger.
tiRandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
tiRandM = liftUid3 tiRandId

-- | Random value in uniform distribution on trigger.
tRandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
tRandM = liftUid3 tRandId

-- | Triggered windex.
tWindexM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
tWindexM = liftUid3 tWindexId

-- | White noise.
whiteNoiseM :: Uid m => Rate -> m Ugen
whiteNoiseM = liftUid1 whiteNoiseId
