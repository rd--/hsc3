> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let loc = mouseX KR (-1) 1 Linear 0.2
>   in X.oteySoundBoard AR (pan2 (soundIn 0) loc 0.1) 20 20 0.8

> g_02 =
>   let d = dust 'α' AR 1
>       n = whiteNoise 'β' AR
>       i = decay (d * 0.5) 0.2 * n
>       loc = mouseX KR (-1) 1 Linear 0.2
>   in X.oteySoundBoard AR (pan2 i loc 0.1) 20 20 0.8
