    Sound.SC3.UGen.Help.viewSC3Help "TwoPole"
    Sound.SC3.UGen.DB.ugenSummary "TwoPole"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = whiteNoise 'α' AR
>     in twoPole (n * 0.005) 2000 0.95

> g_02 =
>     let n = whiteNoise 'α' AR
>         f = xLine KR 800 8000 8 RemoveSynth
>     in twoPole (n * 0.005) f 0.95
