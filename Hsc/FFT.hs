module Hsc.FFT where

import Hsc.UGen

fft  r buf i = UGen r "FFT"  [buf,i] [r] 0 0
ifft r buf   = UGen r "IFFT" [buf]   [r] 0 0
