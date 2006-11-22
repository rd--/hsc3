module Sound.SC3.UGen.Oscillator where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, mkOsc)

-- | Band Limited ImPulse generator.
blip :: Rate -> UGen -> UGen -> UGen
blip r freq nharm = mkOsc r "Blip" [freq, nharm] 1 0

-- | Formant oscillator.
formant :: Rate -> UGen -> UGen -> UGen -> UGen
formant r f0 f bw = mkOsc r "Formant" [f0, f, bw] 1 0

-- | Fast sine wave oscillator implemented using a ringing filter.
fSinOsc :: Rate -> UGen -> UGen -> UGen
fSinOsc r freq phase = mkOsc r "FSinOsc" [freq, phase] 1 0

-- | Impulse oscillator (non band limited).
impulse :: Rate -> UGen -> UGen -> UGen
impulse r freq phase = mkOsc r "Impulse" [freq, phase] 1 0

-- | Pulse oscillator (non band limited).
lfPulse :: Rate -> UGen -> UGen -> UGen -> UGen
lfPulse r freq iphase width = mkOsc r "LFPulse" [freq, iphase, width] 1 0

-- | Sawtooth oscillator (non band limited).
lfSaw :: Rate -> UGen -> UGen -> UGen
lfSaw r freq phase = mkOsc r "LFSaw" [freq, phase] 1 0

-- | Triggered linear ramp between two levels.
phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor r t f s e p = mkOsc r "Phasor" [t, f, s, e, p] 1 0

-- | Pulse wave generator (band limited).
pulse :: Rate -> UGen -> UGen -> UGen
pulse r freq width = mkOsc r "Pulse" [freq, width] 1 0

-- | Sawtooth oscillator (band limited).
saw :: Rate -> UGen -> UGen
saw r freq = mkOsc r "Saw" [freq] 1 0

-- | Sine oscillator.
sinOsc :: Rate -> UGen -> UGen -> UGen
sinOsc r freq phase = mkOsc r "SinOsc" [freq, phase] 1 0

-- | Sawtooth oscillator hard synched to a fundamental. 
syncSaw :: Rate -> UGen -> UGen -> UGen
syncSaw r syncFreq sawFreq = mkOsc r "SyncSaw" [syncFreq, sawFreq] 1 0

-- | Variable duty sawtooth oscillator.
varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
varSaw r freq iphase width = mkOsc r "VarSaw" [freq, iphase, width] 1 0

-- | Three variable wavetable oscillator.
vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vOsc3 r b f1 f2 f3 = mkOsc r "VOsc3" [b, f1, f2, f3] 1 0

-- | Variable wavetable oscillator.
vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc r b f phase = mkOsc r "VOsc" [b, f, phase] 1 0
