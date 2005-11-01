module Hsc.IO where

import Hsc.UGen
import Hsc.MCE

out r b i = UGen r "Out" (b : (forceMCE i)) [] 0
