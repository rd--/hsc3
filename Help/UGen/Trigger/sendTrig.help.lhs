> Sound.SC3.UGen.Help.viewSC3Help "SendTrig"
> Sound.SC3.UGen.DB.ugenSummary "SendTrig"

> import Sound.SC3.ID

> let {s = lfNoise0 'α' KR 5
>     ;o = sinOsc AR (s * 200 + 500) 0 * 0.1}
> in audition (mrg [sendTrig s 0 s,out 0 o])

Retrieve a single message
> withSC3 (withNotifications (waitReply "/tr"))
