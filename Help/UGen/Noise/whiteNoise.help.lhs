> Sound.SC3.UGen.Help.viewSC3Help "WhiteNoise"
> Sound.SC3.UGen.DB.ugenSummary "WhiteNoise"

> import Sound.SC3.ID

> audition (out 0 (whiteNoise 'a' AR * 0.05))

Random filtered noise bursts.
> let {n = whiteNoise 'a' AR
>     ;t = dust 'a' AR (mce [3, 7])
>     ;f = tExpRand 'a' 20 1800 t
>     ;bw = tExpRand 'a' 0.001 1 t
>     ;e = decay2 t 0.01 0.2
>     ;r = resonz (n * e) f bw}
> in audition (out 0 r)

> import qualified Sound.SC3.Monadic as M

Monadic form of above graph.
> do {n <- M.whiteNoise AR
>    ;t <- M.dust AR (mce [3, 7])
>    ;f <- M.tExpRand 20 1800 t
>    ;bw <- M.tExpRand 0.001 1 t
>    ;let {e = decay2 t 0.01 0.2
>         ;r = resonz (n * e) f bw}
>      in audition (out 0 r)}

The same graph again, without using do notation.
> M.whiteNoise AR >>= \n ->
> M.dust AR (mce [3, 7]) >>= \t ->
> M.tExpRand 20 1800 t >>= \f ->
> M.tExpRand 0.001 1 t >>= \bw ->
> let {e = decay2 t 0.01 0.2
>     ;r = resonz (n * e) f bw}
> in audition (out 0 r)
