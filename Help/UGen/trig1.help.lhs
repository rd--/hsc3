> Sound.SC3.UGen.Help.viewSC3Help "Trig1"
> Sound.SC3.UGen.DB.ugenSummary "Trig1"

> import Sound.SC3

> let {d = dust 'α' AR 1
>     ;o = fSinOsc AR 800 0 * 0.2}
> in audition (out 0 (trig1 d 0.2 * o))
