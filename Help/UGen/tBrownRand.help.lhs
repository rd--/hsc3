> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>     let t = dust 'α' KR 10
>         dist = mouseX KR 0 5 Linear 0.2
>         f = X.tBrownRand 'β' 300 3000 1 dist t
>     in sinOsc AR f 0 * 0.1

> g_02 =
>     let t = dust 'α' KR 10
>         n = X.tBrownRand 'β' 0 1 0.2 0 t
>         f = linExp n 0 1 300 3000
>         o = sinOsc AR f 0
>         l = X.tBrownRand 'γ' (-1) 1 1 4 t
>     in pan2 o l 0.1

audio rate noise

> g_03 =
>   let x = mouseX KR 500 5000 Exponential 0.2
>       y = mouseY KR 10 500 Exponential 0.2
>       t = dust 'α' AR x
>   in lag (X.tBrownRand 'β' (-1) 1 0.2 0 t) (y / 48000)
