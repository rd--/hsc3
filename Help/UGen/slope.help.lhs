> Sound.SC3.UGen.Help.viewSC3Help "Slope"
> Sound.SC3.UGen.DB.ugenSummary "Slope"

> import Sound.SC3

> let sig f0 =
>   let {a = lfNoise2 'α' AR f0 {- quadratic noise -}
>       ;b = slope a {- first derivative, line segments -}
>       ;c = slope b {- second derivative, constant segments -}
>       ;s = 0.0002}
>   in [a, b * s, c * s * s]

> let {f = mce (sig 2) * 220 + 220
>     ;o = sinOsc AR f 0 * 0.1}
> in audition (out 0 (mix o))

Drawing

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen 0.05 (mce (sig 2000))
