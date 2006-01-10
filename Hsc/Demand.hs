module Hsc.Demand where

import Hsc.UGen
import Hsc.MCE

dbrown l lo hi step  = UGen DR "Dbrown"  [l, lo, hi, step] [DR] 0 r0
dgeom l start grow   = UGen DR "Dgeom"   [l, start, grow]  [DR] 0 r0
dibrown l lo hi step = UGen DR "Dibrown" [l, lo, hi, step] [DR] 0 r0
diwhite l lo hi      = UGen DR "Diwhite" [l, lo, hi]       [DR] 0 r0
dseries l start step = UGen DR "Dseries" [l, start, step]  [DR] 0 r0
dwhite l lo hi       = UGen DR "Dwhite"  [l, lo, hi]       [DR] 0 r0

drand l array    = proxyU DR "Drand"    (forceMCE array) (replicate l DR) 0 r0
dseq l array     = proxyU DR "Dseq"     (forceMCE array) (replicate l DR) 0 r0
dser l array     = proxyU DR "Dser"     (forceMCE array) (replicate l DR) 0 r0
dswitch1 l array = proxyU DR "Dswitch1" (forceMCE array) (replicate l DR) 0 r0
dxrand l array   = proxyU DR "Dxrand"   (forceMCE array) (replicate l DR) 0 r0
