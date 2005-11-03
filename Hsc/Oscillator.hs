module Hsc.Oscillator where

import Hsc.UGen

impulse r freq phase = UGen r "Impulse" [freq,phase] [r] 0 0
lfsaw  r freq phase  = UGen r "LFSaw"   [freq,phase] [r] 0 0
sinosc r freq phase  = UGen r "SinOsc"  [freq,phase] [r] 0 0

saw r freq = UGen r "Saw" [freq] [r] 0 0

vosc  r bufpos f phase  = UGen r "VOsc"  [bufpos,f,phase]  [r] 0 0
vosc3 r bufpos f1 f2 f3 = UGen r "VOsc3" [bufpos,f1,f2,f3] [r] 0 0
