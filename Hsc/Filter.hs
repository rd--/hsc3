module Hsc.Filter where

import Hsc.UGen

comb' c r i max dly dcy = UGen r "CombN" [i, max, dly, dcy] [r] 0

combn = comb' "CombN"
combl = comb' "CombL"
combc = comb' "CombC"
