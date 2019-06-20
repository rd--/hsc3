> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Common.Math.Filter.BEQ {- hsc3 -}

> g_01 =
>     let i = whiteNoise 'α' AR {- soundIn (mce2 0 1) -}
>         f = mouseX KR 10 20000 Exponential 0.2
>         rq = mouseY KR 0 1 Linear 0.2
>     in bHiPass i f rq

calculate coefficients and use sos (see also bHiPass4)

> g_02 =
>     let i = whiteNoise 'α' AR
>         f = mouseX KR 10 20000 Exponential 0.2
>         rq = mouseY KR 0 1 Linear 0.2
>         (a0, a1, a2, b1, b2) = bHiPassCoef sampleRate f rq
>     in sos i a0 a1 a2 b1 b2
