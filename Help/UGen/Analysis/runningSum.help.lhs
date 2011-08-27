> Sound.SC3.UGen.Help.viewSC3Help "RunningSum"
> Sound.SC3.UGen.DB.ugenSummary "RunningSum"

> import Sound.SC3

> let a = runningSum (in' 1 AR numOutputBuses) 40 * (1/40)
> in audition (out 0 (sinOsc AR 440 0 * a))
