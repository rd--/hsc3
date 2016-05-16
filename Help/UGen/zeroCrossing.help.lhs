> Sound.SC3.UGen.Help.viewSC3Help "ZeroCrossing"
> Sound.SC3.UGen.DB.ugenSummary "ZeroCrossing"

> import Sound.SC3

> let a = sinOsc AR (sinOsc KR 1 0 * 600 + 700) 0 * 0.1
> in audition (out 0 (mce [a, impulse AR (zeroCrossing a) 0 * 0.25]))
