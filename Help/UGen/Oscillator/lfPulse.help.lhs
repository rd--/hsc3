> Sound.SC3.UGen.Help.viewSC3Help "LFPulse"
> Sound.SC3.UGen.DB.ugenSummary "LFPulse"

# SC2 had no initial phase argument.

> import Sound.SC3

> let f = lfPulse KR 3 0 0.3 * 200 + 200
> in audition (out 0 (lfPulse AR f 0 0.2 * 0.1))

> let x = mouseX KR 0 1 Linear 0.2
> in audition (out 0 (lfPulse AR 220 0 x * 0.1))
