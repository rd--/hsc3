    Sound.SC3.UGen.Help.viewSC3Help "BPeakEQ"
    Sound.SC3.UGen.DB.ugenSummary "BPeakEQ"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let i = soundIn 0
>         freq = mouseX KR 2200 18000 Exponential 0.2
>         db = mouseY KR 12 (-12) Linear 0.2
>     in bPeakEQ i freq 0.8 db

> g_02 =
>     let i = soundIn 0
>         freq = mouseX KR 2200 18000 Exponential 0.2
>         rq = mouseY KR 10 0.4 Linear 0.2
>     in bPeakEQ i freq rq 6
