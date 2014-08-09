-- | Standard SC3 graphs, referenced in documentation.
module Sound.SC3.UGen.Help.Graph where

import Sound.SC3.UGen

-- | The SC3 /default/ instrument 'UGen' graph.
default_ugen_graph :: UGen
default_ugen_graph =
    let f = control KR "freq" 440
        a = control KR "amp" 0.1
        p = control KR "pan" 0
        g = control KR "gate" 1
        o = control KR "out" 0
        e = linen g 0.01 0.7 0.3 RemoveSynth
        f3 = mce [f,f + rand 'α' (-0.4) 0,f + rand 'β' 0 0.4]
        l = xLine KR (rand 'γ' 4000 5000) (rand 'δ' 2500 3200) 1 DoNothing
        z = lpf (mix (varSaw AR f3 0 0.3 * 0.3)) l * e
    in out o (pan2 z p a)

-- | A /Gabor/ grain, envelope is by 'lfGauss'.
gabor_grain_ugen_graph :: UGen
gabor_grain_ugen_graph =
    let o = control IR "out" 0
        f = control IR "freq" 440
        d = control IR "sustain" 1
        l = control IR "pan" 1
        a = control IR "amp" 0.1
        w = control IR "width" 0.25
        e = lfGauss AR d w 0 NoLoop RemoveSynth
        s = fSinOsc AR f (0.5 * pi) * e
    in offsetOut o (pan2 s l a)

-- | A /sine/ grain, envelope is by 'envGen' of 'envSine'.
sine_grain_ugen_graph :: UGen
sine_grain_ugen_graph =
    let o = control IR "out" 0
        f = control IR "freq" 440
        d = control IR "sustain" 1
        l = control IR "pan" 1
        a = control IR "amp" 0.1
        w = control IR "width" 0.25
        e = envGen AR 1 1 0 1 DoNothing (envSine (d * w) 1)
        s = fSinOsc AR f (0.5 * pi) * e
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
    let b = control KR "bufnum" 0
        l = control KR "pan" 0
        a = control KR "amp" 0.1
        r = control KR "rate" 1
        m = control KR "rdelay" 0
        v = control KR "ramplitude" 0
        w = control KR "attack" 0
        y = control KR "decay" 0.5
        r' = bufRateScale KR b * r
        p = playBuf 1 AR b r' 1 0 NoLoop RemoveSynth
        e = if use_gate
            then let g = control KR "gate" 1
                 in envGen KR g 1 0 1 RemoveSynth (envASR w 1 y EnvSin)
            else let s = control KR "sustain" 1
                 in envGen KR 1 1 0 1 RemoveSynth (envLinen w s y 1)
        d = delayC (p * e) m (rand 'α' 0 m)
    in out 0 (pan2 d l (a + rand 'β' 0 v))
