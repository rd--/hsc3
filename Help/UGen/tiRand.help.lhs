> import Sound.SC3 {- hsc3 -}

> g_00 =
>   let l = tiRand 'α' (-1) 1 (dust 'β' KR 10)
>   in pan2 (pinkNoise 'γ' AR * 0.1) l 1

> g_01 :: UId m => m UGen
> g_01 = do
>   l <- tiRandM (-1) 1 =<< dustM KR 10
>   n <- pinkNoiseM AR
>   return (pan2 (n * 0.1) l 1)

> g_02 :: UId m => m UGen
> g_02 = do
>   n <- tiRandM 4 12 =<< dustM KR 10
>   let f = n * 150 + (mce [0,1])
>   return (sinOsc AR f 0 * 0.1)
