> Sound.SC3.UGen.Help.viewSC3Help "Gate"
> Sound.SC3.UGen.DB.ugenSummary "Gate"

# hsc3: filter

> import Sound.SC3

> let t = lfPulse AR 1 0 0.1
> in audition (out 0 (gate (fSinOsc AR 500 0 * 0.25) t))
