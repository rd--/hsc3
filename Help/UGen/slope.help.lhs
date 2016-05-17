    Sound.SC3.UGen.Help.viewSC3Help "Slope"
    Sound.SC3.UGen.DB.ugenSummary "Slope"

> import Sound.SC3 {- hsc3 -}

> sig f0 =
>     let a = lfNoise2 'α' AR f0 {- quadratic noise -}
>         b = slope a {- first derivative, line segments -}
>         c = slope b {- second derivative, constant segments -}
>         s = 0.0002
>     in [a, b * s, c * s * s]

> g_01 =
>     let f = mce (sig 2) * 220 + 220
>     in mix (sinOsc AR f 0 * 0.1)

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen 0.05 (mce (sig 2000))

![](sw/hsc3/Help/SVG/slope.0.svg)
