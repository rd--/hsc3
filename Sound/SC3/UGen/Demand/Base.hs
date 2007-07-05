module Sound.SC3.UGen.Demand.Base where

import Sound.SC3.UGen.Rate (Rate(DR))
import Sound.SC3.UGen.UGen (UGen, UGenID, mkOsc, mkOscMCE)
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Private

-- | Buffer demand ugen.
dbufrd :: UGen -> UGen -> Loop -> (UGenID -> UGen)
dbufrd b p l = mkOsc DR "Dbufrd" [b, p, fromLoop l] 1

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> (UGenID -> UGen)
dwhite l lo hi = mkOsc DR "Dwhite" [l, lo, hi] 1

-- | Demand rate integer white noise.
diwhite :: UGen -> UGen -> UGen -> (UGenID -> UGen)
diwhite l lo hi = mkOsc DR "Diwhite" [l, lo, hi] 1

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> (UGenID -> UGen)
dbrown l lo hi step = mkOsc DR "Dbrown" [l, lo, hi, step] 1

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> (UGenID -> UGen)
dibrown l lo hi step = mkOsc DR "Dibrown" [l, lo, hi, step] 1

-- | Demand rate random selection.
drand :: UGen -> UGen -> (UGenID -> UGen)
drand l array = mkOscMCE DR "Drand" [l] array 1

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGen -> UGen -> (UGenID -> UGen)
dxrand l array = mkOscMCE DR "Dxrand" [l] array 1

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> (UGenID -> UGen)
dseries l i n = mkOsc DR "Dseries" [l, i, n] 1

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> (UGenID -> UGen)
dgeom l i n = mkOsc DR "Dgeom" [l, i, n] 1

-- | Demand rate sequence generator.
dseq :: UGen -> UGen -> (UGenID -> UGen)
dseq l array = mkOscMCE DR "Dseq" [l] array 1

-- | Demand rate series generator.
dser :: UGen -> UGen -> (UGenID -> UGen)
dser l array = mkOscMCE DR "Dser" [l] array 1

-- | Demand rate input switching.
dswitch1 :: UGen -> UGen -> (UGenID -> UGen)
dswitch1 l array = mkOscMCE DR "Dswitch1" [l] array 1
