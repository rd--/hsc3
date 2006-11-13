module Sound.SC3.UGen.Demand where

import Sound.SC3.UGen.Rate (Rate(DR))
import Sound.SC3.UGen.UGen (UGen, mkOsc, mkOscMCE)

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> UGen
dwhite l lo hi = mkOsc DR "Dwhite" [l, lo, hi] 1 0

-- | Demand rate integer white noise.
diwhite l lo hi = mkOsc DR "Diwhite" [l, lo, hi] 1 0
diwhite :: UGen -> UGen -> UGen -> UGen

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> UGen
dbrown l lo hi step = mkOsc DR "Dbrown" [l, lo, hi, step] 1 0

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> UGen
dibrown l lo hi step = mkOsc DR "Dibrown" [l, lo, hi, step] 1 0

-- | Demand rate random selection.
drand :: Int -> UGen -> UGen
drand l array = mkOscMCE DR "Drand" [] array l 0

-- | Demand rate random selection with no immediate repetition.
dxrand :: Int -> UGen -> UGen
dxrand l array = mkOscMCE DR "Dxrand" [] array l 0

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> UGen
dseries l start step = mkOsc DR "Dseries" [l, start, step] 1 0

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> UGen
dgeom l start grow = mkOsc DR "Dgeom" [l, start, grow] 1 0

-- | Demand rate sequence generator.
dseq :: Int -> UGen -> UGen
dseq l array = mkOscMCE DR "Dseq" [] array l 0

dser :: Int -> UGen -> UGen
dser l array = mkOscMCE DR "Dser" [] array l 0

-- | Demand rate input switching.
dswitch1 :: Int -> UGen -> UGen
dswitch1 l array = mkOscMCE DR "Dswitch1" [] array l 0
