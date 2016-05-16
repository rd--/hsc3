> Sound.SC3.UGen.Help.viewSC3Help "PulseDivider"
> Sound.SC3.UGen.DB.ugenSummary "PulseDivider"

> import Sound.SC3

> let {p = impulse AR 8 0
>     ;d = pulseDivider p (mce [4,7]) 0
>     ;a = sinOsc AR 1200 0 * decay2 p 0.005 0.1
>     ;b = sinOsc AR 600  0 * decay2 d 0.005 0.5}
> in audition (out 0 (a + b * 0.4))
