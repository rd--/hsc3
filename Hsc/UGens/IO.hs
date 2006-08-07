module Hsc.UGens.IO where

import Hsc.Construct

in' n r bus = mkOsc r "In" [bus] n 0

keyState r key min max lag = mkOsc r "KeyState" [key,min,max,lag] 1 0

mouseButton r min max lag  = mkOsc r "MouseButton"  [min,max,lag] 1 0
mouseX r min max warp lag  = mkOsc r "MouseX"  [min,max,warp,lag] 1 0
mouseY r min max warp lag  = mkOsc r "MouseY"  [min,max,warp,lag] 1 0

out b i        = mkFilterMCE "Out"        [b] i 0 0
replaceOut b i = mkFilterMCE "ReplaceOut" [b] i 0 0
