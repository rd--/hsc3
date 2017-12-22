    Sound.SC3.UGen.Help.viewSC3Help "T2K"
    Sound.SC3.UGen.DB.ugenSummary "T2K"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let tr = t2k (dust 'α' AR 4)
>   in trig tr 0.1 * sinOsc AR 800 0 * 0.1
