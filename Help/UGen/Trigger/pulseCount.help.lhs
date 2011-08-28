> Sound.SC3.UGen.Help.viewSC3Help "PulseCount"
> Sound.SC3.UGen.DB.ugenSummary "PulseCount"

> import Sound.SC3

> let c = pulseCount (impulse AR 10 0) (impulse AR 0.4 0)
> in audition (out 0 (sinOsc AR (c * 200) 0 * 0.05))
