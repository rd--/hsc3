module Sound.SC3.UGen.Demand.Monadic where

import Sound.SC3.UGen.UGen (UGen, uniquify, zeroUId)
import qualified Sound.SC3.UGen.Demand.Pure as D

-- | Buffer demand ugen.
dbufrd :: UGen -> UGen -> UGen -> IO UGen
dbufrd b p l = uniquify (D.dbufrd zeroUId b p l)

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> IO UGen
dwhite l lo hi = uniquify (D.dwhite zeroUId l lo hi)

-- | Demand rate integer white noise.
diwhite :: UGen -> UGen -> UGen -> IO UGen
diwhite l lo hi = uniquify (D.diwhite zeroUId l lo hi)

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> IO UGen
dbrown l lo hi step = uniquify (D.dbrown zeroUId l lo hi step)

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> IO UGen
dibrown l lo hi step = uniquify (D.dibrown zeroUId l lo hi step)

-- | Demand rate random selection.
drand :: UGen -> UGen -> IO UGen
drand l array = uniquify (D.drand zeroUId l array)

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGen -> UGen -> IO UGen
dxrand l array = uniquify (D.dxrand zeroUId l array)

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> IO UGen
dseries l i n = uniquify (D.dseries zeroUId l i n)

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> IO UGen
dgeom l i n = uniquify (D.dgeom zeroUId l i n)

-- | Demand rate sequence generator.
dseq :: UGen -> UGen -> IO UGen
dseq l array = uniquify (D.dseq zeroUId l array)

-- | Demand rate series generator.
dser :: UGen -> UGen -> IO UGen
dser l array = uniquify (D.dser zeroUId l array)

-- | Demand rate input switching.
dswitch1 :: UGen -> UGen -> IO UGen
dswitch1 l array = uniquify (D.dswitch1 zeroUId l array)
