    > Sound.SC3.UGen.Help.viewSC3Help "HenonN"
    > Sound.SC3.UGen.DB.ugenSummary "HenonN"

> import Sound.SC3 {- hsc3 -}

With SC3 default initial parameters.

> g_01 =
>     let x = mouseX KR 20 sampleRate Linear 0.1
>     in henonN AR x 1.4 0.3 0 0 * 0.1

With mouse-control of parameters.

> g_02 =
>     let x = mouseX KR 1 1.4 Linear 0.1
>         y = mouseY KR 0 0.3 Linear 0.1
>     in henonN AR (sampleRate / 4) x y 0 0 * 0.1

With randomly modulated parameters.

> g_03 =
>     let n0 = lfNoise2 'α' KR 1 * 0.20 + 1.20
>         n1 = lfNoise2 'β' KR 1 * 0.15 + 0.15
>     in henonN AR (sampleRate / 8) n0 n1 0 0 * 0.1

As a frequency control.

> g_04 =
>     let x = mouseX KR 1 1.4 Linear 0.1
>         y = mouseY KR 0 0.3 Linear 0.1
>         f0 = 40
>         f = henonN AR f0 x y 0 0 * 800 + 900
>     in sinOsc AR f 0 * 0.4

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (henonN AR 2500 1.4 0.3 0 0 * 0.1)

![](sw/hsc3/Help/SVG/henonN.0.svg)
