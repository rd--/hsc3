    Sound.SC3.UGen.Help.viewSC3Help "Streson"
    Sound.SC3.UGen.DB.ugenSummary "Streson"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> f_01 z =
>   let dt = recip (linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 280 377)
>   in streson z dt 0.9 * 0.3

> g_01 = f_01 (lfSaw AR (mce2 220 180) 0 * 0.2)

> g_02 = f_01 (soundIn 0)
