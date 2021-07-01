-- | Standard SC3 graphs, referenced in documentation.
module Sound.SC3.UGen.Help.Graph where

import Sound.SC3.Common.Enum
import Sound.SC3.Common.Envelope
import Sound.SC3.Common.Rate

import Sound.SC3.UGen.Bindings
import Sound.SC3.UGen.Mce
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | The SC3 /default/ instrument 'UGen' graph.
default_ugen_graph :: UGen
default_ugen_graph =
    let f = control kr "freq" 440
        a = control kr "amp" 0.1
        p = control kr "pan" 0
        g = control kr "gate" 1
        o = control kr "out" 0
        e = linen g 0.01 0.7 0.3 RemoveSynth
        f3 = mce [f,f + randId 'α' (-0.4) 0,f + randId 'β' 0 0.4]
        l = xLine kr (randId 'γ' 4000 5000) (randId 'δ' 2500 3200) 1 DoNothing
        z = lpf (mix (varSaw ar f3 0 0.3 * 0.3)) l * e
    in out o (pan2 z p a)

-- | A /Gabor/ grain, envelope is by 'lfGauss'.
gabor_grain_ugen_graph :: UGen
gabor_grain_ugen_graph =
    let o = control ir "out" 0
        f = control ir "freq" 440
        d = control ir "sustain" 1
        l = control ir "pan" 0
        a = control ir "amp" 0.1
        w = control ir "width" 0.25
        e = lfGauss ar d w 0 NoLoop RemoveSynth
        s = fSinOsc ar f (0.5 * pi) * e
    in offsetOut o (pan2 s l a)

-- | A /sine/ grain, envelope is by 'envGen' of 'envSine'.
sine_grain_ugen_graph :: UGen
sine_grain_ugen_graph =
    let o = control ir "out" 0
        f = control ir "freq" 440
        d = control ir "sustain" 1
        l = control ir "pan" 0
        a = control ir "amp" 0.1
        w = control ir "width" 0.25
        e = envGen ar 1 1 0 1 RemoveSynth (envSine (d * w) 1)
        s = fSinOsc ar f (0.5 * pi) * e
    in offsetOut o (pan2 s l a)

-- | Trivial file playback instrument.
--
-- If /use_gate/ is 'True' there is a /gate/ parameter and the synth
-- ends either when the sound file ends or the gate closes, else there
-- is a /sustain/ parameter to indicate the duration.  In both cases a
-- linear envelope with a decay time of /decay/ is applied.
--
-- The /rdelay/ parameter sets the maximum pre-delay time (in
-- seconds), each instance is randomly pre-delayed between zero and
-- the indicated time.  The /ramplitude/ parameter sets the maximum
-- amplitude offset of the /amp/ parameter, each instance is randomly
-- amplified between zero and the indicated value.
default_sampler_ugen_graph :: Bool -> UGen
default_sampler_ugen_graph use_gate =
    let b = control kr "bufnum" 0
        l = control kr "pan" 0
        a = control kr "amp" 0.1
        r = control kr "rate" 1
        m = control kr "rdelay" 0
        v = control kr "ramplitude" 0
        w = control kr "attack" 0
        y = control kr "decay" 0.5
        r' = bufRateScale kr b * r
        p = playBuf 1 ar b r' 1 0 NoLoop RemoveSynth
        e = if use_gate
            then let g = control kr "gate" 1
                 in envGen kr g 1 0 1 RemoveSynth (envASR w 1 y EnvSin)
            else let s = control kr "sustain" 1
                 in envGen kr 1 1 0 1 RemoveSynth (envLinen w s y 1)
        d = delayC (p * e) m (randId 'α' 0 m)
    in out 0 (pan2 d l (a + randId 'β' 0 v))
