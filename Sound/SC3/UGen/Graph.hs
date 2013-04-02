-- | Standard SC3 graphs, referenced in documentation.
module Sound.SC3.UGen.Graph where

import Sound.SC3.UGen.ID

-- | The SC3 /default/ instrument 'UGen' graph.
default_ugen_graph :: UGen
default_ugen_graph =
    let f = control KR "freq" 440
        a = control KR "amp" 0.1
        p = control KR "pan" 0
        g = control KR "gate" 1
        e = linen g 0.01 0.7 0.3 RemoveSynth
        f3 = mce [f,f + rand 'a' (-0.4) 0,f + rand 'b' 0 0.4]
        l = xLine KR (rand 'c' 4000 5000) (rand 'd' 2500 3200) 1 DoNothing
        z = lpf (mix (varSaw AR f3 0 0.3 * 0.3)) l * e
    in out 0 (pan2 z p a)

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
