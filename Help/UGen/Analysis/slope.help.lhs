> Sound.SC3.UGen.Help.viewSC3Help "Slope"
> Sound.SC3.UGen.DB.ugenSummary "Slope"

> import Sound.SC3.ID

> let {a = lfNoise2 'a' KR 2 {- quadratic noise -}
>     ;s = 1/2
>     ;b = slope a * s {- first derivative, line segments -}
>     ;c = slope b * squared s {- second derivative, constant segments -}
>     ;f = mce [a, b, c] * 220 + 220
>     ;o = sinOsc AR f 0 * (1/3)}
> in audition (out 0 (mix o))
