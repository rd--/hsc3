    > Sound.SC3.UGen.Help.viewSC3Help "BLowPass"
    > Sound.SC3.UGen.DB.ugenSummary "BLowPass"
    > :t bLowPassCoef

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let i = soundIn (mce2 0 1)
>         f = mouseX KR 10 20000 Exponential 0.2
>         rq = mouseY KR 0 1 Linear 0.2
>     in bLowPass i f rq
>
> g_02 =
>     let i = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.3)
>         f = mouseX KR 100 20000 Exponential 0.2
>         rq = mouseY KR 0.1 1 Linear 0.2
>     in bLowPass i f rq

Calculate coefficients and use sos.

> g_03 =
>     let i = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.3)
>         f = mouseX KR 100 20000 Exponential 0.2
>         rq = mouseY KR 0.1 1 Linear 0.2
>         (a0, a1, a2, b1, b2) = bLowPassCoef sampleRate f rq
>         flt ip = sos ip a0 a1 a2 b1 b2
>     in flt (flt i)
