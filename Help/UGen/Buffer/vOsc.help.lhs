> Sound.SC3.UGen.Help.viewSC3Help "VOsc"
> Sound.SC3.UGen.DB.ugenSummary "VOsc"

> import Sound.SC3

Allocate and fill tables 0 to 7.
> let {square a = a * a
>     ;bf = [Normalise,Wavetable,Clear]
>     ;harm i = let {n = square (i + 1)
>                   ;f j = square ((n - j) / n)}
>                in map f [0 .. n - 1]
>     ;setup i = do {i' <- return (fromIntegral i)
>                   ;_ <- async (b_alloc i 1024 1)
>                   ;send (b_gen_sine1 i bf (harm i'))}}
> in withSC3 (mapM_ setup [0 .. 7])

Oscillator at buffers 0 through 7, mouse selects buffer.
> let x = mouseX KR 0 7 Linear 0.1
> in audition (out 0 (vOsc AR x (mce [120, 121]) 0 * 0.3))

> import Sound.SC3.Lang.Random.IO

Reallocate buffers while oscillator is running.
> let {bf = [Normalise,Wavetable,Clear]
>     ;resetTable i = do {h <- nrrand 12 0 1
>                        ;send (b_gen_sine1 i bf h)}}
> in withSC3 (mapM_ resetTable [0 .. 7])
