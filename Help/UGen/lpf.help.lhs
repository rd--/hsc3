    > Sound.SC3.UGen.Help.viewSC3Help "LPF"
    > Sound.SC3.UGen.DB.ugenSummary "LPF"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let f = xLine KR 0.7 300 20 RemoveSynth
>         ff = fSinOsc KR f 0 * 3600 + 4000
>     in lpf (saw AR 200 * 0.1) ff

Control rate filtering.

> g_02 =
>     let ctl = lpf (lfPulse KR 8 0 0.5) (mouseX KR 2 50 Exponential 0.1)
>     in sinOsc AR (ctl * 200 + 400) 0 * 0.1
