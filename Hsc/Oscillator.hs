module Hsc.Oscillator where

import Hsc.UGen

impulse r freq phase = UGen r "Impulse" [freq,phase] [r] 0 0
lfsaw  r freq phase  = UGen r "LFSaw"   [freq,phase] [r] 0 0
sinosc r freq phase  = UGen r "SinOsc"  [freq,phase] [r] 0 0
