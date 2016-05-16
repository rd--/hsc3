> Sound.SC3.UGen.Help.viewSC3Help "LagUD"
> Sound.SC3.UGen.DB.ugenSummary "LagUD"

> import Sound.SC3

lag pitch, slower down (5 seconds) than up (1 second)

> let {x = mouseX KR 220 440 Linear 0.2
>     ;o = sinOsc AR (mce2 x (lagUD x 1 5)) 0 * 0.1}
> in audition (out 0 o)
