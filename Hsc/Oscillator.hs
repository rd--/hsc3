module Hsc.Oscillator where

import Hsc.UGen

sinosc r freq phase =  UGen r "SinOsc" [freq, phase] [r] 0
lfsaw  r freq phase =  UGen r "LFSaw"  [freq, phase] [r] 0
