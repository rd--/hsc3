    Sound.SC3.UGen.Help.viewSC3Help "Gate"
    Sound.SC3.UGen.DB.ugenSummary "Gate"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let t = lfPulse AR 1 0 0.1
>   in gate (fSinOsc AR 500 0 * 0.25) t
