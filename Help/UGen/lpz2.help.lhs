    Sound.SC3.UGen.Help.viewSC3Help "LPZ2"
    Sound.SC3.UGen.DB.ugenSummary "LPZ2"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = whiteNoise 'α' AR
>   in lpz2 (n * 0.25)
