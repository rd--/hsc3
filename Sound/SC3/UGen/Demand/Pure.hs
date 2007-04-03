module Sound.SC3.UGen.Demand.Pure where

import Sound.SC3.UGen.UGen (UGen, withUId)
import Sound.SC3.UGen.UId  (UId)
import qualified Sound.SC3.UGen.Demand.Base as D

-- | Buffer demand ugen.
dbufrd :: UId -> UGen -> UGen -> UGen -> UGen
dbufrd d b p l = withUId d (D.dbufrd b p l)

-- | Demand rate white noise.
dwhite :: UId -> UGen -> UGen -> UGen -> UGen
dwhite d l lo hi = withUId d (D.dwhite l lo hi)

-- | Demand rate integer white noise.
diwhite :: UId -> UGen -> UGen -> UGen -> UGen
diwhite d l lo hi = withUId d (D.diwhite l lo hi)

-- | Demand rate brown noise.
dbrown :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown d l lo hi step = withUId d (D.dbrown l lo hi step)

-- | Demand rate integer brown noise.
dibrown :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown d l lo hi step = withUId d (D.dibrown l lo hi step)

-- | Demand rate random selection.
drand :: UId -> UGen -> UGen -> UGen
drand d l array = withUId d (D.drand l array)

-- | Demand rate random selection with no immediate repetition.
dxrand :: UId -> UGen -> UGen -> UGen
dxrand d l array = withUId d (D.dxrand l array)

-- | Demand rate arithmetic series.
dseries :: UId -> UGen -> UGen -> UGen -> UGen
dseries d l i n = withUId d (D.dseries l i n)

-- | Demand rate geometric series.
dgeom :: UId -> UGen -> UGen -> UGen -> UGen
dgeom d l i n = withUId d (D.dgeom l i n)

-- | Demand rate sequence generator.
dseq :: UId -> UGen -> UGen -> UGen
dseq d l array = withUId d (D.dseq l array)

-- | Demand rate series generator.
dser :: UId -> UGen -> UGen -> UGen
dser d l array = withUId d (D.dser l array)

-- | Demand rate input switching.
dswitch1 :: UId -> UGen -> UGen -> UGen
dswitch1 d l array = withUId d (D.dswitch1 l array)
