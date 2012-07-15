> Sound.SC3.UGen.Help.viewSC3Help "BBandPass"
> Sound.SC3.UGen.DB.ugenSummary "BBandPass"

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 20 20000 Exponential 0.2
>     ; bw = mouseY KR 0 10 Linear 0.2 }
> in audition (out 0 (bBandPass i f bw))
