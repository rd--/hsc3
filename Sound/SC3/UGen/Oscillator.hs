module Sound.SC3.UGen.Oscillator where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, mkOsc)

impulse, lfSaw, sinOsc, fSinOsc :: Rate -> UGen -> UGen -> UGen
impulse r freq phase = mkOsc r "Impulse" [freq,phase] 1 0
lfSaw   r freq phase = mkOsc r "LFSaw"   [freq,phase] 1 0
sinOsc  r freq phase = mkOsc r "SinOsc"  [freq,phase] 1 0
fSinOsc r freq phase = mkOsc r "FSinOsc" [freq,phase] 1 0

saw :: Rate -> UGen -> UGen
saw r freq           = mkOsc r "Saw"     [freq]       1 0

vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc  r b f phase    = mkOsc r "VOsc"    [b,f,phase]  1 0

vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vOsc3 r b f1 f2 f3   = mkOsc r "VOsc3"   [b,f1,f2,f3] 1 0

formant :: Rate -> UGen -> UGen -> UGen -> UGen
formant r f0 f bw    = mkOsc r "Formant"  [f0,f,bw] 1 0

pulse, blip :: Rate -> UGen -> UGen -> UGen
pulse r freq width   = mkOsc r "Pulse" [freq,width] 1 0
blip  r freq nharm   = mkOsc r "Blip"  [freq,nharm] 1 0

lfPulse, varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
lfPulse r freq iphase width = mkOsc r "LFPulse" [freq,iphase,width] 1 0
varSaw  r freq iphase width = mkOsc r "VarSaw"  [freq,iphase,width] 1 0

phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor r t f s e p = mkOsc r "Phasor" [t,f,s,e,p] 1 0
