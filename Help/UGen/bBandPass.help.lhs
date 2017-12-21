    Sound.SC3.UGen.Help.viewSC3Help "BBandPass"
    Sound.SC3.UGen.DB.ugenSummary "BBandPass"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Common.Math.Filter.BEQ {- hsc3 -}

> g_01 =
>     let i = soundIn 0
>         f = mouseX KR 20 20000 Exponential 0.2
>         bw = mouseY KR 0 10 Linear 0.2
>     in bBandPass i f bw

calculate coefficients and use sos

> g_02 =
>     let i = soundIn 0
>         f = mouseX KR 20 20000 Exponential 0.2
>         bw = mouseY KR 0 10 Linear 0.2
>         (a0, a1, a2, b1, b2) = bBandPassCoef sampleRate f bw
>     in sos i a0 a1 a2 b1 b2
