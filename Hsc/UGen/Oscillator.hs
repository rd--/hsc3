module Hsc.UGen.Oscillator where

import Hsc.Construct (mkOsc)
import Hsc.Rate (Rate)
import Hsc.UGen (UGen)

impulse r freq phase = mkOsc r "Impulse" [freq,phase] 1 0
lfSaw   r freq phase = mkOsc r "LFSaw"   [freq,phase] 1 0
sinOsc  r freq phase = mkOsc r "SinOsc"  [freq,phase] 1 0
fSinOsc r freq phase = mkOsc r "FSinOsc" [freq,phase] 1 0

saw     r freq       = mkOsc r "Saw"     [freq]       1 0

vOsc  r b f phase    = mkOsc r "VOsc"    [b,f,phase]  1 0
vOsc3 r b f1 f2 f3   = mkOsc r "VOsc3"   [b,f1,f2,f3] 1 0

formant r f0 f bw  = mkOsc r "Formant"  [f0,f,bw] 1 0
pulse r freq width = mkOsc r "Pulse" [freq,width] 1 0
blip r freq nharm  = mkOsc r "Blip"  [freq,nharm] 1 0

lfPulse r freq iphase width = mkOsc r "LFPulse" [freq,iphase,width] 1 0
varSaw  r freq iphase width = mkOsc r "VarSaw"  [freq,iphase,width] 1 0

phasor r t f s e p = mkOsc r "Phasor" [t,f,s,e,p] 1 0


blip :: Rate -> UGen -> UGen -> UGen
fSinOsc :: Rate -> UGen -> UGen -> UGen
formant :: Rate -> UGen -> UGen -> UGen -> UGen
impulse :: Rate -> UGen -> UGen -> UGen
lfPulse :: Rate -> UGen -> UGen -> UGen -> UGen
lfSaw :: Rate -> UGen -> UGen -> UGen
phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pulse :: Rate -> UGen -> UGen -> UGen
saw :: Rate -> UGen -> UGen
sinOsc :: Rate -> UGen -> UGen -> UGen
vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
