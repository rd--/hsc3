> Sound.SC3.UGen.Help.viewSC3Help "BHiShelf"
> Sound.SC3.UGen.DB.ugenSummary "BHiShelf"

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 2200 18000 Exponential 0.2
>     ; db = mouseY KR 18 (-18) Linear 0.2 }
> in audition (out 0 (bHiShelf i f 1 db))

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 2200 18000 Exponential 0.2
>     ; rs = mouseY KR 0.1 1 Linear 0.2 }
> in audition (out 0 (bHiShelf i f rs 6))
