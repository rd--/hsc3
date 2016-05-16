    Sound.SC3.UGen.Help.viewSC3Help "LFClipNoise"
    Sound.SC3.UGen.DB.ugenSummary "LFClipNoise"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = lfClipNoise 'α' AR 1000 * 0.05

Modulate frequency

> g_02 =
>     let f = xLine KR 1000 10000 10 RemoveSynth
>     in lfClipNoise 'α' AR f * 0.05

Use as frequency control

> g_03 =
>     let n = lfClipNoise 'α' KR 4
>     in sinOsc AR (n * 200 + 600) 0 * 0.1

Drawings

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (lfClipNoise 'α' AR 1000)

![](sw/hsc3/Help/SVG/lfClipNoise.0.svg)
