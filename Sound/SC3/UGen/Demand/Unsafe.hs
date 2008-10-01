module Sound.SC3.UGen.Demand.Unsafe where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Lift
import qualified Sound.SC3.UGen.Demand.Monadic as D
import Sound.SC3.UGen.Enum

-- | Buffer demand ugen.
dbufrd :: UGen -> UGen -> Loop -> UGen
dbufrd = liftP3 D.dbufrd

-- | Buffer write on demand unit generator.
dbufwr :: UGen -> UGen -> UGen -> Loop -> UGen
dbufwr = liftP4 D.dbufwr

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> UGen
dwhite = liftP3 D.dwhite

-- | Demand rate integer white noise.
diwhite :: UGen -> UGen -> UGen -> UGen
diwhite = liftP3 D.diwhite

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> UGen
dbrown = liftP4 D.dbrown

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> UGen
dibrown = liftP4 D.dibrown

-- | Demand rate random selection.
drand :: UGen -> UGen -> UGen
drand = liftP2 D.drand

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGen -> UGen -> UGen
dxrand = liftP2 D.dxrand

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> UGen
dseries = liftP3 D.dseries

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> UGen
dgeom = liftP3 D.dgeom

-- | Demand rate sequence generator.
dseq :: UGen -> UGen -> UGen
dseq = liftP2 D.dseq

-- | Demand rate series generator.
dser :: UGen -> UGen -> UGen
dser = liftP2 D.dser

-- | Demand input replication
dstutter :: UGen -> UGen -> UGen
dstutter = liftP2 D.dstutter

-- | Demand rate input switching.
dswitch1 :: UGen -> UGen -> UGen
dswitch1 = liftP2 D.dswitch1

-- | Demand rate input switching.
dswitch :: UGen -> UGen -> UGen
dswitch = liftP2 D.dswitch
