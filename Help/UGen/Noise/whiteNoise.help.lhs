> Sound.SC3.UGen.Help.viewSC3Help "WhiteNoise"
> Sound.SC3.UGen.DB.ugenSummary "WhiteNoise"

> import Sound.SC3

> audition (out 0 (whiteNoise 'α' AR * 0.05))

Random filtered noise bursts.

> let {n = whiteNoise 'α' AR
>     ;t = dust 'β' AR (mce [3, 7])
>     ;f = tExpRand 'γ' 20 1800 t
>     ;bw = tExpRand 'δ' 0.001 1 t
>     ;e = decay2 t 0.01 0.2
>     ;r = resonz (n * e) f bw}
> in audition (out 0 r)

Monadic form of above graph.

> do {n <- whiteNoiseM AR
>    ;t <- dustM AR (mce [3, 7])
>    ;f <- tExpRandM 20 1800 t
>    ;bw <- tExpRandM 0.001 1 t
>    ;let {e = decay2 t 0.01 0.2
>         ;r = resonz (n * e) f bw}
>      in audition (out 0 r)}

The same graph again, without using do notation.

> whiteNoiseM AR >>= \n ->
> dustM AR (mce [3, 7]) >>= \t ->
> tExpRandM 20 1800 t >>= \f ->
> tExpRandM 0.001 1 t >>= \bw ->
> let {e = decay2 t 0.01 0.2
>     ;r = resonz (n * e) f bw}
> in audition (out 0 r)

Drawing

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.01 (whiteNoise 'γ' AR)
> plot_ugen1 0.05 (lpf (whiteNoise 'γ' AR) 500)

Speaker balance

> let n = whiteNoise 'α' AR * 0.05 in audition (out 0 (mce2 n n))

> let {x = mouseX KR 0.1 2 Linear 0.2
>     ;l = sinOsc KR x 0
>     ;n = whiteNoise 'α' AR}
> in audition (out 0 (pan2 n l 0.05))
