    Sound.SC3.UGen.Help.viewSC3Help "TwoZero"
    Sound.SC3.UGen.DB.ugenSummary "TwoZero"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = whiteNoise 'α' AR
>         f = xLine KR 20 20000 8 RemoveSynth
>     in twoZero (n * 0.125) f 1
