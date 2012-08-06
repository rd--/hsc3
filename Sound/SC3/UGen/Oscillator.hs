-- | Oscillators.
module Sound.SC3.UGen.Oscillator where

import Data.List
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Band Limited ImPulse generator.
blip :: Rate -> UGen -> UGen -> UGen
blip r freq nharm = mkOscR [AR] r "Blip" [freq, nharm] 1

-- | Chorusing wavetable oscillator.
cOsc :: Rate -> UGen -> UGen -> UGen -> UGen
cOsc r n f b = mkOsc r "COsc" [n,f,b] 1

-- | Create a constant amplitude signal.
dc :: Rate -> UGen -> UGen
dc r k = mkOsc r "DC" [k] 1

-- | Formant oscillator.
formant :: Rate -> UGen -> UGen -> UGen -> UGen
formant r f0 f bw = mkOscR [AR] r "Formant" [f0, f, bw] 1

-- | Fast sine wave oscillator implemented using a ringing filter.
fSinOsc :: Rate -> UGen -> UGen -> UGen
fSinOsc r freq phase = mkOsc r "FSinOsc" [freq, phase] 1

-- | Dynamic stochastic synthesis generator conceived by Iannis Xenakis.
gendy1 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gendy1 r ampDist durDist adParam ddParam minFreq maxFreq ampScale durScale initCPs kNum = mkOsc r "Gendy1" [ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum] 1

-- | Impulse oscillator (non band limited).
impulse :: Rate -> UGen -> UGen -> UGen
impulse r freq phase = mkOsc r "Impulse" [freq, phase] 1

-- | Bank of fixed oscillators.
klang :: Rate -> UGen -> UGen -> UGen -> UGen
klang r fs fo a =
    if r == AR
    then mkOscMCE r "Klang" [fs, fo] a 1
    else error "klang: not AR"

-- | Format frequency, amplitude and phase data as required for klang.
klangSpec :: [UGen] -> [UGen] -> [UGen] -> UGen
klangSpec f a p = mce ((concat . transpose) [f, a, p])

-- | Variant of 'klangSpec' for non-UGen inputs.
klangSpec' :: [Double] -> [Double] -> [Double] -> UGen
klangSpec' f a p =
    let u = map constant
    in klangSpec (u f) (u a) (u p)

-- | Variant of 'klangSpec' for 'MCE' inputs.
klangSpec_mce :: UGen -> UGen -> UGen -> UGen
klangSpec_mce f a p =
    let m = mceChannels
    in klangSpec (m f) (m a) (m p)

-- | A sine like shape made of two cubic pieces.
lfCub :: Rate -> UGen -> UGen -> UGen
lfCub r freq phase = mkOsc r "LFCub" [freq, phase] 1

-- | Gaussian function oscillator
lfGauss ::  Rate -> UGen -> UGen -> UGen -> Loop -> DoneAction -> UGen
lfGauss r duration width iphase loop doneAction = mkOscR [AR,KR] r "LFGauss" [duration,width,iphase,from_loop loop,from_done_action doneAction] 1

-- | A sine like shape made of two cubic pieces.
lfPar :: Rate -> UGen -> UGen -> UGen
lfPar r freq phase = mkOsc r "LFPar" [freq, phase] 1

-- | Pulse oscillator (non band limited).
lfPulse :: Rate -> UGen -> UGen -> UGen -> UGen
lfPulse r freq iphase width = mkOsc r "LFPulse" [freq, iphase, width] 1

-- | Sawtooth oscillator (non band limited).
lfSaw :: Rate -> UGen -> UGen -> UGen
lfSaw r freq phase = mkOsc r "LFSaw" [freq, phase] 1

-- | Sawtooth oscillator (non band limited).
lfTri :: Rate -> UGen -> UGen -> UGen
lfTri r freq phase = mkOsc r "LFTri" [freq, phase] 1

-- | Triggered linear ramp between two levels.
phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor r t f s e p = mkOsc r "Phasor" [t, f, s, e, p] 1

-- | Pulse wave generator (band limited).
pulse :: Rate -> UGen -> UGen -> UGen
pulse r freq width = mkOscR [AR] r "Pulse" [freq, width] 1

-- | Sawtooth oscillator (band limited).
saw :: Rate -> UGen -> UGen
saw r freq = mkOscR [AR,KR] r "Saw" [freq] 1

-- | Sine oscillator.
sinOsc :: Rate -> UGen -> UGen -> UGen
sinOsc r freq phase = mkOsc r "SinOsc" [freq, phase] 1

-- | Feedback FM oscillator.
sinOscFB :: Rate -> UGen -> UGen -> UGen
sinOscFB r freq feedback = mkOscR [AR,KR] r "SinOscFB" [freq,feedback] 1

-- | Sawtooth oscillator hard synched to a fundamental.
syncSaw :: Rate -> UGen -> UGen -> UGen
syncSaw r syncFreq sawFreq = mkOsc r "SyncSaw" [syncFreq, sawFreq] 1

-- | Variable duty sawtooth oscillator.
varSaw :: Rate -> UGen -> UGen -> UGen -> UGen
varSaw r freq iphase width = mkOsc r "VarSaw" [freq, iphase, width] 1

-- | The Vibrato oscillator models a slow frequency modulation.
vibrato :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vibrato r freq rate depth delay onset rateVariation depthVariation iphase = mkOscR [AR,KR] r "Vibrato" [freq,rate,depth,delay,onset,rateVariation,depthVariation,iphase] 1

-- Local Variables:
-- truncate-lines:t
-- End:
