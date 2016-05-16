    Sound.SC3.UGen.Help.viewSC3Help "Dust"
    Sound.SC3.UGen.DB.ugenSummary "Dust"

> import Sound.SC3
>
> g_01 = dust 'α' AR 200 * 0.25
>
> g_02 =
>     let d = xLine KR 20000 2 10 RemoveSynth
>     in dust 'β' AR d * 0.15

Drawings

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (dust 'γ' AR 300)
    plot_ugen1 0.1 (dust 'γ' AR (xLine KR 5000 100 0.1 RemoveSynth))

![](sw/hsc3/Help/SVG/dust.0.svg)
![](sw/hsc3/Help/SVG/dust.1.svg)

Illustrate monadic constructor

> g_03,g_04 :: UId m => m UGen
> g_03 = fmap (* 0.25) (dustM AR 200)
> g_04 = fmap (* 0.15) (dustM AR (xLine KR 20000 2 10 RemoveSynth))
