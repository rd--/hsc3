> Sound.SC3.UGen.Help.viewSC3Help "ControlDur"
> Sound.SC3.UGen.DB.ugenSummary "ControlDur"

> import Sound.SC3

controlRate and controlDur are reciprocals

> let f = mce2 controlRate (recip controlDur)
> in audition (out 0 (sinOsc AR f 0 * 0.1))
