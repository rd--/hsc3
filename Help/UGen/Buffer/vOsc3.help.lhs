> Sound.SC3.UGen.Help.viewSC3Help "VOsc3"
> Sound.SC3.UGen.DB.ugenSummary "VOsc3"

> import Sound.SC3

allocate and fill tables 0 to 7.
> let {square a = a * a
>     ;bf = [Normalise,Wavetable,Clear]
>     ;harm i = let {n = square (i + 1)
>                   ;f j = square ((n - j) / n)}
>                in map f [0 .. n - 1]
>     ;setup i = do {i' <- return (fromIntegral i)
>                   ;_ <- async (b_alloc i 1024 1)
>                   ;send (b_gen_sine1 i bf (harm i'))}}
> in withSC3 (mapM_ setup [0 .. 7])

oscillator at buffers 0 through 7, mouse selects buffer.
> let {x = mouseX KR 0 7 Linear 0.1
>     ;o1 = vOsc3 AR x 120 121 129
>     ;o2 = vOsc3 AR x 119 123 127}
> in audition (out 0 (mce2 o1 o2 * 0.3))
