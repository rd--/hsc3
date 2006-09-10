module Sound.SC3.UGen.Demand where

import Sound.SC3.UGen.Rate (Rate(DR))
import Sound.SC3.UGen.UGen (UGen, mkOsc, mkOscMCE)

dbrown l lo hi step  = mkOsc DR "Dbrown"  [l, lo, hi, step] 1 0
dgeom l start grow   = mkOsc DR "Dgeom"   [l, start, grow]  1 0
dibrown l lo hi step = mkOsc DR "Dibrown" [l, lo, hi, step] 1 0
diwhite l lo hi      = mkOsc DR "Diwhite" [l, lo, hi]       1 0
dseries l start step = mkOsc DR "Dseries" [l, start, step]  1 0
dwhite l lo hi       = mkOsc DR "Dwhite"  [l, lo, hi]       1 0

drand l array    = mkOscMCE DR "Drand"    [] array l 0
dseq l array     = mkOscMCE DR "Dseq"     [] array l 0
dser l array     = mkOscMCE DR "Dser"     [] array l 0
dswitch1 l array = mkOscMCE DR "Dswitch1" [] array l 0
dxrand l array   = mkOscMCE DR "Dxrand"   [] array l 0

dbrown :: UGen -> UGen -> UGen -> UGen -> UGen
dgeom :: UGen -> UGen -> UGen -> UGen
dibrown :: UGen -> UGen -> UGen -> UGen -> UGen
diwhite :: UGen -> UGen -> UGen -> UGen
drand :: Int -> UGen -> UGen
dseq :: Int -> UGen -> UGen
dser :: Int -> UGen -> UGen
dseries :: UGen -> UGen -> UGen -> UGen
dswitch1 :: Int -> UGen -> UGen
dwhite :: UGen -> UGen -> UGen -> UGen
dxrand :: Int -> UGen -> UGen
