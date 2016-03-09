    Sound.SC3.UGen.Help.viewSC3Help "Amplitude"
    Sound.SC3.UGen.DB.ugenSummary "Amplitude"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let s = soundIn 4
>         a = amplitude KR s 0.01 0.01
>     in pulse AR 90 0.3 * a

> g_02 =
>     let s = soundIn 4
>         f = amplitude KR s 0.5 0.5 * 1200 + 400
>     in sinOsc AR f 0 * 0.1

> g_03 =
>     let s = soundIn 4
>         a = amplitude AR s 0.5 0.05
>     in s * a
