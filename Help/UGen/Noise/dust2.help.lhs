    Sound.SC3.UGen.Help.viewSC3Help "Dust2"
    Sound.SC3.UGen.DB.ugenSummary "Dust2"

> import Sound.SC3 {- hsc3 -}

> g_01 = dust2 'α' AR 200 * 0.5

> g_02 =
>     let d = xLine KR 20000 2 10 RemoveSynth
>     in dust2 'β' AR d * 0.15

Drawings

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (dust2 'γ' AR 400)
    plot_ugen1 0.1 (dust2 'γ' AR (xLine KR 5000 100 0.1 RemoveSynth))

![](sw/hsc3/Help/SVG/dust2.0.svg)
![](sw/hsc3/Help/SVG/dust2.1.svg)
