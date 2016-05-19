    > Sound.SC3.UGen.Help.viewSC3Help "WhiteNoise"
    > Sound.SC3.UGen.DB.ugenSummary "WhiteNoise"

> import Sound.SC3 {- hsc3 -}

> g_01 = whiteNoise 'α' AR * 0.05

Random filtered noise bursts.

> g_02 =
>     let n = whiteNoise 'α' AR
>         t = dust 'β' AR (mce [3, 7])
>         f = tExpRand 'γ' 20 1800 t
>         bw = tExpRand 'δ' 0.001 1 t
>         e = decay2 t 0.01 0.2
>     in resonz (n * e) f bw

Monadic form of above graph.

> g_03 :: UId m => m UGen
> g_03 = do
>   n <- whiteNoiseM AR
>   t <- dustM AR (mce [3, 7])
>   f <- tExpRandM 20 1800 t
>   bw <- tExpRandM 0.001 1 t
>   let e = decay2 t 0.01 0.2
>   return (resonz (n * e) f bw)

The same graph again, without using do notation.

> g_04 :: UId m => m UGen
> g_04 =
>     whiteNoiseM AR >>= \n ->
>     dustM AR (mce [3, 7]) >>= \t ->
>     tExpRandM 20 1800 t >>= \f ->
>     tExpRandM 0.001 1 t >>= \bw ->
>     let e = decay2 t 0.01 0.2
>     in return (resonz (n * e) f bw)

Drawing

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.01 (whiteNoise 'γ' AR)
    plot_ugen1 0.05 (lpf (whiteNoise 'γ' AR) 500)

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.1 (whiteNoise 'α' AR)

![](sw/hsc3/Help/SVG/whiteNoise.0.svg)
![](sw/hsc3/Help/SVG/whiteNoise.1.svg)
![](sw/hsc3/Help/SVG/whiteNoise.2.svg)

Speaker balance

> g_05 = let n = whiteNoise 'α' AR * 0.05 in mce2 n n

> g_06 =
>     let x = mouseX KR 0.1 2 Linear 0.2
>         l = sinOsc KR x 0
>         n = whiteNoise 'α' AR
>     in pan2 n l 0.05
