    Sound.SC3.UGen.Help.viewSC3Help "LFNoise2"
    Sound.SC3.UGen.DB.ugenSummary "LFNoise2"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = lfNoise2 'α' AR 1000 * 0.05

Modulate frequency.

> g_02 =
>     let f = xLine KR 1000 10000 10 RemoveSynth
>     in lfNoise2 'α' AR f * 0.05

Use as frequency control.

> g_03 =
>     let f = lfNoise2 'α' KR 4
>     in sinOsc AR (f * 400 + 450) 0 * 0.1

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (lfNoise2 'α' AR 1000)

![](sw/hsc3/Help/SVG/lfNoise2.0.svg)
