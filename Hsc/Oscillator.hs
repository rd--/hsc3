module Hsc.Oscillator where

import Hsc.UGen

impulse r freq phase = UGen r "Impulse" [freq,phase] [r] 0 r0
lfsaw   r freq phase = UGen r "LFSaw"   [freq,phase] [r] 0 r0
sinosc  r freq phase = UGen r "SinOsc"  [freq,phase] [r] 0 r0
fsinosc r freq phase = UGen r "FSinOsc" [freq,phase] [r] 0 r0

saw     r freq       = UGen r "Saw"     [freq]       [r] 0 r0

vosc  r b f phase    = UGen r "VOsc"    [b,f,phase]  [r] 0 r0
vosc3 r b f1 f2 f3   = UGen r "VOsc3"   [b,f1,f2,f3] [r] 0 r0

formant r f0 f bw = UGen r "Formant" [f0,f,bw] [r] 0 r0

pulse r freq width = UGen r "Pulse" [freq,width] [r] 0 r0

blip r freq nharm = UGen r "Blip" [freq,nharm] [r] 0 r0
