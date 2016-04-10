    Sound.SC3.UGen.Help.viewSC3Help "TExpRand"
    Sound.SC3.UGen.DB.ugenSummary "TExpRand"

> import Sound.SC3 {- hsc3 -}

> g_01 :: UId m => m UGen
> g_01 = do
>  f <- tExpRandM 300.0 3000.0 =<< dustM KR 10
>  return (sinOsc AR f 0 * 0.1)
