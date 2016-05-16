    Sound.SC3.UGen.Help.viewSC3Help "ClipNoise"
    Sound.SC3.UGen.DB.ugenSummary "ClipNoise"

> import Sound.SC3
>
> g_01 = clipNoise 'α' AR * 0.1 {- hsc3 -}

Drawings

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.01 (clipNoise 'α' AR)

![](sw/hsc3/Help/SVG/clipNoise.0.svg)
