> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Common.Math.Noise {- hsc3 -}

SC3 default initial parameters.

> g_01 =
>   let x = mouseX KR 20 sampleRate Linear 0.1
>   in latoocarfianC AR x 1 3 0.5 0.5 0.5 0.5 * 0.2

Randomly modulate all parameters.

> g_02 =
>   let [n0,n1,n2,n3] = map (\e -> lfNoise2 e KR 5) "abcd"
>       f = sampleRate / 4
>       a = n0 * 1.5 + 1.5
>       b = n1 * 1.5 + 1.5
>       c = n2 * 0.5 + 1.5
>       d = n3 * 0.5 + 1.5
>   in latoocarfianC AR f a b c d 0.5 0.5 * 0.2

Haskell implementation of equation.

> latoocarfian_hs a b c d = map fst (iterate (latoocarfian_f a b c d) (0.5,0.5))

    import Sound.SC3.Plot {- hsc3-plot -}
    plotTable1 (take 600 (latoocarfian_hs 1.0 3.0 0.5 0.5))
    plot_ugen_nrt (600,1) 1.0 (latoocarfianC AR 600 1.0 3.0 0.5 0.5 0.5 0.5)
