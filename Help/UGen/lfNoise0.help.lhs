    Sound.SC3.UGen.Help.viewSC3Help "LFNoise0"
    Sound.SC3.UGen.DB.ugenSummary "LFNoise0"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = lfNoise0 'α' AR 1000 * 0.05

Modulate frequency.

> g_02 =
>     let f = xLine KR 1000 10000 10 RemoveSynth
>     in lfNoise0 'α' AR f * 0.05

Use as frequency control.

> g_03 =
>     let f = lfNoise0 'α' KR 4
>     in sinOsc AR (f * 400 + 450) 0 * 0.1

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (lfNoise0 'γ' AR 1000)
    > plot_ugen1 0.1 (lfNoise1 'γ' AR 1000)
    > plot_ugen1 0.1 (lfNoise2 'γ' AR 1000)

![](sw/hsc3/Help/SVG/lfNoise0.0.svg)
![](sw/hsc3/Help/SVG/lfNoise0.1.svg)
![](sw/hsc3/Help/SVG/lfNoise0.2.svg)
