    Sound.SC3.UGen.Help.viewSC3Help "TIRand"
    Sound.SC3.UGen.DB.ugenSummary "TIRand"

> import Sound.SC3 {- hsc3 -}

> g_01 :: UId m => m UGen
> g_01 = do
>   l <- tIRandM (-1) 1 =<< dustM KR 10
>   n <- pinkNoiseM AR
>   return (pan2 (n * 0.1) l 1)

> g_02 :: UId m => m UGen
> g_02 = do
>   n <- tIRandM 4 12 =<< dustM KR 10
>   let f = n * 150 + (mce [0,1])
>   return (sinOsc AR f 0 * 0.1)
