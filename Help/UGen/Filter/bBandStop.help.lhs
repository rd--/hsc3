> Sound.SC3.UGen.Help.viewSC3Help "BBandStop"
> Sound.SC3.UGen.DB.ugenSummary "BBandStop"

> import Sound.SC3

> let {i = soundIn (mce2 0 1)
>     ;f = mouseX' KR 20 20000 Exponential 0.2
>     ;bw = mouseY' KR 0 10 Linear 0.2}
> in audition (out 0 (bBandStop i f bw))

> let {i = sinOsc AR 1000 (mce2 0 0)
>     ;f = mouseX' KR 800 1200 Exponential 0.2
>     ;bw = mouseY' KR 0 10 Linear 0.2}
> in audition (out 0 (bBandStop i f bw))
