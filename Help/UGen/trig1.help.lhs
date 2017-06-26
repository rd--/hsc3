    Sound.SC3.UGen.Help.viewSC3Help "Trig1"
    Sound.SC3.UGen.DB.ugenSummary "Trig1"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let d = dust 'α' AR 1
>         o = fSinOsc AR 800 0 * 0.2
>     in o * trig1 d 0.2
