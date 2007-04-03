module Sound.SC3.UGen.Demand.Monadic where

import Sound.SC3.UGen.UGen (UGen, uniquify)
import qualified Sound.SC3.UGen.Demand.Base as D

-- | Buffer demand ugen.
dbufrd :: UGen -> UGen -> UGen -> IO UGen
dbufrd b p l = uniquify (D.dbufrd b p l)

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> IO UGen
dwhite l lo hi = uniquify (D.dwhite l lo hi)

-- | Demand rate integer white noise.
diwhite :: UGen -> UGen -> UGen -> IO UGen
diwhite l lo hi = uniquify (D.diwhite l lo hi)

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> IO UGen
dbrown l lo hi step = uniquify (D.dbrown l lo hi step)

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> IO UGen
dibrown l lo hi step = uniquify (D.dibrown l lo hi step)

-- | Demand rate random selection.
drand :: UGen -> UGen -> IO UGen
drand l array = uniquify (D.drand l array)

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGen -> UGen -> IO UGen
dxrand l array = uniquify (D.dxrand l array)

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> IO UGen
dseries l i n = uniquify (D.dseries l i n)

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> IO UGen
dgeom l i n = uniquify (D.dgeom l i n)

-- | Demand rate sequence generator.
dseq :: UGen -> UGen -> IO UGen
dseq l array = uniquify (D.dseq l array)

-- | Demand rate series generator.
dser :: UGen -> UGen -> IO UGen
dser l array = uniquify (D.dser l array)

-- | Demand rate input switching.
dswitch1 :: UGen -> UGen -> IO UGen
dswitch1 l array = uniquify (D.dswitch1 l array)
