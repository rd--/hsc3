    > Sound.SC3.UGen.Help.viewSC3Help "TRand"
    > Sound.SC3.UGen.DB.ugenSummary "TRand"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let t = dust 'α' KR (mce2 5 12)
>         f = tRand 'β' (mce2 200 1600) (mce2 500 3000) t
>     in sinOsc AR f 0 * 0.2

> g_02_m = do
>   t <- dustM KR (mce2 5 12)
>   f <- tRandM (mce2 200 1600) (mce2 500 3000) t
>   return (sinOsc AR f 0 * 0.2)

> g_02 = uid_st_eval g_02_m
