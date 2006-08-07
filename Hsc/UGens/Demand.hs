module Hsc.UGens.Demand where

import Hsc.UGen ()
import Hsc.Rate (Rate(DR))
import Hsc.Construct (mkOsc, mkOscMCE)

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
