> Sound.SC3.UGen.Help.viewSC3Help "Timer"
> Sound.SC3.UGen.DB.ugenSummary "Timer"

> import Sound.SC3

> let t = impulse KR (mouseX KR 0.5 20 Exponential 0.1) 0
> in audition (out 0 (sinOsc AR (timer t * 500 + 500) 0 * 0.2))
