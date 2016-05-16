> Sound.SC3.UGen.Help.viewSC3Help "Trig"
> Sound.SC3.UGen.DB.ugenSummary "Trig"

> import Sound.SC3

> let {d = dust 'α' AR 1
>     ;o = fSinOsc AR 800 0 * 0.5}
> in audition (out 0 (trig d 0.2 * o))
