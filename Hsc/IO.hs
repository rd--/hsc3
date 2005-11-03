module Hsc.IO where

import Hsc.UGen
import Hsc.MCE

in' n r bus = UGen r "In" [bus] (replicate n r) 0 0
in1 = in' 1
in2 = in' 2
in4 = in' 4
in8 = in' 8

mousebutton r min max lag = UGen r "MouseButton" [min,max,lag] [r] 0 0
mousex r min max warp lag = UGen r "MouseX" [min,max,warp,lag] [r] 0 0
mousey r min max warp lag = UGen r "MouseY" [min,max,warp,lag] [r] 0 0

out r b i = UGen r "Out" (b : (forceMCE i)) [] 0 0
