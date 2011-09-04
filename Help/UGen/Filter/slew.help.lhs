> Sound.SC3.UGen.Help.viewSC3Help "Slew"
> Sound.SC3.UGen.DB.ugenSummary "Slew"

> import Sound.SC3

> let z = lfPulse AR 800 0 0.5 * 0.2
> in audition (out 0 (mce2 z (slew z 4000 4000)))

> let z = saw AR 800 * 0.2
> in audition (out 0 (mce2 z (slew z 400 400)))
