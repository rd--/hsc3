    > Sound.SC3.UGen.Help.viewSC3Help "VOsc"
    > Sound.SC3.UGen.DB.ugenSummary "VOsc"

> import Sound.SC3 {- hsc3 -}

Allocate and fill tables 0 to 7.

> square a = a * a
> bf = [Normalise,Wavetable,Clear]
> gen_harm i =
>     let n = square (i + 1)
>         f j = square ((n - j) / n)
>     in map f [0 .. n - 1]
> gen_setup i =
>     let i' = fromIntegral i
>     in (b_alloc i 1024 1,b_gen_sine1 i bf (gen_harm i'))
> run_setup (p,q) = (async p >> send q)

    > withSC3 (mapM_ (run_setup . gen_setup) [0 .. 7])

Oscillator at buffers 0 through 7, mouse selects buffer.

> g_01 =
>     let x = mouseX KR 0 7 Linear 0.1
>         y = mouseY KR 0.01 0.2 Exponential 0.2
>     in vOsc AR x (mce [120, 121]) 0 * y

Reallocate buffers while oscillator is running.

    > import Sound.SC3.Lang.Random.IO {- hsc3-lang -}
    >
    > let resetTable i = do {h <- nrrand 12 0 1
    >                        ;send (b_gen_sine1 i bf h)}
    > in withSC3 (mapM_ resetTable [0 .. 7])
