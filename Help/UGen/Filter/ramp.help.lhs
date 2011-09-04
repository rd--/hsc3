> Sound.SC3.UGen.Help.viewSC3Help "Ramp"
> Sound.SC3.UGen.DB.ugenSummary "Ramp"

> import Sound.SC3

Used to lag pitch
> let {o = lfPulse KR 4 0 0.5 * 50 + 400
>     ;l = line KR 0 1 15 DoNothing
>     ;f = ramp o l}
> in audition (out 0 (sinOsc AR f 0 * 0.3))
