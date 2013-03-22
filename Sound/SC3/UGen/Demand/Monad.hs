-- | Monad constructors for demand 'UGen's, see also
-- "Sound.SC3.UGen.Demand.ID".
module Sound.SC3.UGen.Demand.Monad where

import Sound.SC3.UGen.Demand.ID as D
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UId

-- | Buffer demand ugen.
dbufrd :: (UId m) => UGen -> UGen -> Loop -> m UGen
dbufrd = liftUId3 D.dbufrd

-- | Buffer write on demand unit generator.
dbufwr :: (UId m) => UGen -> UGen -> UGen -> Loop -> m UGen
dbufwr = liftUId4 D.dbufwr

-- | Demand rate white noise.
dwhite :: (UId m) => UGen -> UGen -> UGen -> m UGen
dwhite = liftUId3 D.dwhite

-- | Demand rate integer white noise.
diwhite :: (UId m) => UGen -> UGen -> UGen -> m UGen
diwhite = liftUId3 D.diwhite

-- | Demand rate brown noise.
dbrown :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
dbrown = liftUId4 D.dbrown

-- | Demand rate integer brown noise.
dibrown :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
dibrown = liftUId4 D.dibrown

-- | Demand rate random selection.
drand :: (UId m) => UGen -> UGen -> m UGen
drand = liftUId2 D.drand

-- | Demand rate random selection with no immediate repetition.
dxrand :: (UId m) => UGen -> UGen -> m UGen
dxrand = liftUId2 D.dxrand

-- | Demand rate weighted random sequence generator.
dwrand :: (UId m) => UGen -> UGen -> UGen -> m UGen
dwrand = liftUId3 D.dwrand

-- | Demand rate arithmetic series.
dseries :: (UId m) => UGen -> UGen -> UGen -> m UGen
dseries = liftUId3 D.dseries

-- | Demand rate geometric series.
dgeom :: (UId m) => UGen -> UGen -> UGen -> m UGen
dgeom = liftUId3 D.dgeom

-- | Demand rate sequence generator.
dseq :: (UId m) => UGen -> UGen -> m UGen
dseq = liftUId2 D.dseq

-- | Demand rate series generator.
dser :: (UId m) => UGen -> UGen -> m UGen
dser = liftUId2 D.dser

-- | Demand rate sequence shuffler.
dshuf :: (UId m) => UGen -> UGen -> m UGen
dshuf = liftUId2 D.dshuf

-- | Demand input replication
dstutter :: (UId m) => UGen -> UGen -> m UGen
dstutter = liftUId2 D.dstutter

-- | Demand rate input switching.
dswitch1 :: (UId m) => UGen -> UGen -> m UGen
dswitch1 = liftUId2 D.dswitch1

-- | Demand rate input switching.
dswitch :: (UId m) => UGen -> UGen -> m UGen
dswitch = liftUId2 D.dswitch
