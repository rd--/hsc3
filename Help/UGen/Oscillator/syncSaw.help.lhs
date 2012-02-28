> Sound.SC3.UGen.Help.viewSC3Help "SyncSaw"
> Sound.SC3.UGen.DB.ugenSummary "SyncSaw"

> import Sound.SC3

> let f = line KR 100 800 12 RemoveSynth
> in audition (out 0 (syncSaw AR 100 f * 0.1))

Mouse control
> let {sy_f = mouseY KR 80 220 Exponential 0.2
>     ;sw_f = sy_f * mouseX KR 1 3 Linear 0.2}
> in audition (out 0 (syncSaw AR sy_f sw_f * 0.1))
