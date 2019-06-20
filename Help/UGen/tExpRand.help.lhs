> import Sound.SC3 {- hsc3 -}

> g_00 =
>  let f = tExpRand 'α' 300.0 3000.0 (dust 'β' KR 10)
>  in sinOsc AR f 0 * 0.1

> g_01 :: UId m => m UGen
> g_01 = do
>  f <- tExpRandM 300.0 3000.0 =<< dustM KR 10
>  return (sinOsc AR f 0 * 0.1)
