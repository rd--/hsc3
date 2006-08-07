module Hsc.UGens.Envelope where

import Hsc.Construct (mkOsc, mkFilter)

envGen r gate lvl bias scale done env = mkOsc r "EnvGen" i 1 0
    where i = [gate,lvl,bias,scale,done] ++ env

xLine r start end dur done = mkOsc r "XLine" [start,end,dur,done] 1 0
line  r start end dur done = mkOsc r "Line"  [start,end,dur,done] 1 0

freeSelf  i = mkFilter "FreeSelf"  [i] 1 0
pauseSelf i = mkFilter "PauseSelf" [i] 1 0
