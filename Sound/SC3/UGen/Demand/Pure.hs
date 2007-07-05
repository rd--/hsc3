module Sound.SC3.UGen.Demand.Pure where

import Sound.SC3.UGen.UGen
import qualified Sound.SC3.UGen.Demand.Base as D
import Sound.SC3.UGen.Enum

-- | Buffer demand ugen.
dbufrd :: UGenID -> UGen -> UGen -> Loop -> UGen
dbufrd = liftD3 D.dbufrd

-- | Demand rate white noise.
dwhite :: UGenID -> UGen -> UGen -> UGen -> UGen
dwhite = liftD3 D.dwhite

-- | Demand rate integer white noise.
diwhite :: UGenID -> UGen -> UGen -> UGen -> UGen
diwhite = liftD3 D.diwhite

-- | Demand rate brown noise.
dbrown :: UGenID -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown = liftD4 D.dbrown

-- | Demand rate integer brown noise.
dibrown :: UGenID -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown = liftD4 D.dibrown

-- | Demand rate random selection.
drand :: UGenID -> UGen -> UGen -> UGen
drand = liftD2 D.drand

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGenID -> UGen -> UGen -> UGen
dxrand = liftD2 D.dxrand

-- | Demand rate arithmetic series.
dseries :: UGenID -> UGen -> UGen -> UGen -> UGen
dseries = liftD3 D.dseries

-- | Demand rate geometric series.
dgeom :: UGenID -> UGen -> UGen -> UGen -> UGen
dgeom = liftD3 D.dgeom

-- | Demand rate sequence generator.
dseq :: UGenID -> UGen -> UGen -> UGen
dseq = liftD2 D.dseq

-- | Demand rate series generator.
dser :: UGenID -> UGen -> UGen -> UGen
dser = liftD2 D.dser

-- | Demand rate input switching.
dswitch1 :: UGenID -> UGen -> UGen -> UGen
dswitch1 = liftD2 D.dswitch1
