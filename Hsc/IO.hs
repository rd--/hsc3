module Hsc.IO where

import Hsc.UGen
import Hsc.MCE

in' n r bus = UGen r "In" [bus] (replicate n r) 0 0
in1 = in' 1
in2 = in' 2
in4 = in' 4
in8 = in' 8

out r b i = UGen r "Out" (b : (forceMCE i)) [] 0 0
