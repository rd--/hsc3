> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Allocate and fill tables 0 to 7.

> b_flags = [Normalise,Wavetable,Clear]
> gen_harm i =
>     let square a = a * a
>         n = square (fromIntegral i + 1)
>         f j = square ((n - j) / n)
>     in map f [0 .. n - 1]
> gen_setup i = (b_alloc i 1024 1,b_gen_sine1 i b_flags (gen_harm i))
> run_setup (p,q) = (async p >> sendMessage q)

    import Sound.SC3.Plot {- hsc3-plot -}
    plotImpulses [gen_harm 7]
    withSC3 (mapM_ (run_setup . gen_setup) [0 .. 7])

Oscillator at buffers 0 through 7, mouse selects buffer.

> f_01 k n =
>     let x = mouseX KR k (k + n - 1) Linear 0.1
>         y = mouseY KR 0.01 0.2 Exponential 0.2
>     in vOsc AR x (mce [120, 121]) 0 * y

> g_01 = f_01 0 8

    > audition (f_01 0 24)

Reallocate buffers while oscillator is running.

    > import Sound.SC3.Lang.Random.IO {- hsc3-lang -}
    >
    > resetTable i = do {h <- nrrand 12 0 1;sendMessage (b_gen_sine1 i b_flags h)}
    > withSC3 (mapM_ resetTable [0 .. 7])
