> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Common.Math.Filter.BEQ {- hsc3 -}

thoughpass

> g_01 =
>     let i = soundIn (mce2 0 1)
>         f = mouseX KR 10 18000 Exponential 0.2
>     in bAllPass i f 0.8

bandpass

> g_02 =
>     let i = soundIn (mce2 0 1) * 0.5
>         f = mouseX KR 100 18000 Exponential 0.2
>     in bAllPass i f 0.8 + negate i

calculate coefficients and use sos

> g_03 =
>     let i = soundIn (mce2 0 1) * 0.5
>         f = mouseX KR 100 18000 Exponential 0.2
>         (a0, a1, a2, b1, b2) = bAllPassCoef sampleRate f 0.8
>     in sos i a0 a1 a2 b1 b2 + negate i
