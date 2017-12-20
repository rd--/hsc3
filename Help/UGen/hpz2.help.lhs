    Sound.SC3.UGen.Help.viewSC3Help "HPZ2"
    Sound.SC3.UGen.DB.ugenSummary "HPZ2"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = whiteNoise 'α' AR
>   in hpz2 (n * 0.25)
