   Sound.SC3.UGen.Help.viewSC3Help "BPZ2"
   Sound.SC3.UGen.DB.ugenSummary "BPZ2"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = whiteNoise 'α' AR
>     in bpz2 (n * 0.25)
