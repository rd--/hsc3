> Sound.SC3.UGen.Help.viewSC3Help "BPeakEQ"
> Sound.SC3.UGen.DB.ugenSummary "BPeakEQ"

> import Sound.SC3.ID

> let { i = soundIn 4
>     ; f = mouseX KR 2200 18000 Exponential 0.2
>     ; db = mouseY KR 12 (-12) Linear 0.2 }
> in audition (out 0 (bPeakEQ i f 0.8 db))

> let { i = soundIn 4
>     ; f = mouseX KR 2200 18000 Exponential 0.2
>     ; rq = mouseY KR 10 0.4 Linear 0.2 }
> in audition (out 0 (bPeakEQ i f rq 6))
