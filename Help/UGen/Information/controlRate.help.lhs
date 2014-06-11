> Sound.SC3.UGen.Help.viewSC3Help "ControlRate"
> Sound.SC3.UGen.DB.ugenSummary "ControlRate"

> import Sound.SC3

play a sine tone at control rate, the reciprocal of controlDur

> let f = mce2 controlRate (recip controlDur)
> in audition (out 0 (sinOsc AR f 0 * 0.1))
