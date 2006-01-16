module Hsc.Demand where

import Hsc.UGen
import Hsc.Rate
import Hsc.Construct

dbrown l lo hi step  = mkOsc DR "Dbrown"  [l, lo, hi, step] 1 0 r0
dgeom l start grow   = mkOsc DR "Dgeom"   [l, start, grow]  1 0 r0
dibrown l lo hi step = mkOsc DR "Dibrown" [l, lo, hi, step] 1 0 r0
diwhite l lo hi      = mkOsc DR "Diwhite" [l, lo, hi]       1 0 r0
dseries l start step = mkOsc DR "Dseries" [l, start, step]  1 0 r0
dwhite l lo hi       = mkOsc DR "Dwhite"  [l, lo, hi]       1 0 r0

drand l array    = mkOscMCE DR "Drand"    [] array l 0 r0
dseq l array     = mkOscMCE DR "Dseq"     [] array l 0 r0
dser l array     = mkOscMCE DR "Dser"     [] array l 0 r0
dswitch1 l array = mkOscMCE DR "Dswitch1" [] array l 0 r0
dxrand l array   = mkOscMCE DR "Dxrand"   [] array l 0 r0
