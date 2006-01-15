module Hsc.FFT where

import Hsc.UGen
import Hsc.Construct

fft  buf i = mkOsc KR "FFT"  [buf,i] 1 0 r0
ifft buf   = mkOsc AR "IFFT" [buf]   1 0 r0
