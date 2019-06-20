> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Common.Math.Filter.BEQ {- hsc3 -}

> g_01 =
>     let i = soundIn 0
>         freq = mouseX KR 2200 18000 Exponential 0.2
>         db = mouseY KR 12 (-12) Linear 0.2
>     in bPeakEQ i freq 0.8 db

> g_02 =
>     let i = soundIn 0
>         freq = mouseX KR 2200 18000 Exponential 0.2
>         rq = mouseY KR 10 0.4 Linear 0.2
>     in bPeakEQ i freq rq 6

calculate coefficients and use sos (see also bLowPass4)

> g_03 =
>     let i = soundIn 0
>         freq = mouseX KR 2200 18000 Exponential 0.2
>         rq = mouseY KR 10 0.4 Linear 0.2
>         (a0, a1, a2, b1, b2) = bPeakEQCoef sampleRate freq rq 6
>     in sos i a0 a1 a2 b1 b2
