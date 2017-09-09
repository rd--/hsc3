    Sound.SC3.UGen.Help.viewSC3Help "Decay2"
    Sound.SC3.UGen.DB.ugenSummary "Decay2"

> import Sound.SC3 {- hsc3 -}

Used as an envelope

> g_01 =
>     let s = fSinOsc AR 600 0 * 0.25 -- sinOsc AR 11000 0 * 0.25
>         f = xLine KR 1 50 20 RemoveSynth
>     in decay2 (impulse AR f 0) 0.01 0.2 * s

Compare the above with Decay used as the envelope.

> g_02 =
>     let s = fSinOsc AR 600 0 * 0.25
>         f = xLine KR 1 50 20 RemoveSynth
>     in decay (impulse AR f 0) 0.2 * s

Drawings, attack and decay are a difference of two decays, hence inversion

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.05 (decay2 (impulse AR 1 0) 0.001 0.01)
    > plot_ugen1 0.05 (decay2 (impulse AR 1 0) 0.01 0.001)

![](sw/hsc3/Help/SVG/decay2.0.svg)
![](sw/hsc3/Help/SVG/decay2.1.svg)
