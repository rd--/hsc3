module Sound.SC3.UGen.Demand.Base where

import Sound.SC3.UGen.Rate (Rate(DR))
import Sound.SC3.UGen.UGen (UGen, mkOsc, mkOscMCE)

-- | Buffer demand ugen.
dbufrd :: UGen -> UGen -> UGen -> UGen
dbufrd b p l = mkOsc DR "Dbufrd" [b, p, l] 1 0

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> UGen
dwhite l lo hi = mkOsc DR "Dwhite" [l, lo, hi] 1 0

-- | Demand rate integer white noise.
diwhite :: UGen -> UGen -> UGen -> UGen
diwhite l lo hi = mkOsc DR "Diwhite" [l, lo, hi] 1 0

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> UGen
dbrown l lo hi step = mkOsc DR "Dbrown" [l, lo, hi, step] 1 0

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> UGen
dibrown l lo hi step = mkOsc DR "Dibrown" [l, lo, hi, step] 1 0

-- | Demand rate random selection.
drand :: UGen -> UGen -> UGen
drand l array = mkOscMCE DR "Drand" [l] array 1 0

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGen -> UGen -> UGen
dxrand l array = mkOscMCE DR "Dxrand" [l] array 1 0

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> UGen
dseries l i n = mkOsc DR "Dseries" [l, i, n] 1 0

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> UGen
dgeom l i n = mkOsc DR "Dgeom" [l, i, n] 1 0

-- | Demand rate sequence generator.
dseq :: UGen -> UGen -> UGen
dseq l array = mkOscMCE DR "Dseq" [l] array 1 0

-- | Demand rate series generator.
dser :: UGen -> UGen -> UGen
dser l array = mkOscMCE DR "Dser" [l] array 1 0

-- | Demand rate input switching.
dswitch1 :: UGen -> UGen -> UGen
dswitch1 l array = mkOscMCE DR "Dswitch1" [l] array 1 0
