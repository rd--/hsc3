> Sound.SC3.UGen.Help.viewSC3Help "Slew"
> Sound.SC3.UGen.DB.ugenSummary "Slew"

> import Sound.SC3

> let z = lfPulse AR 800 0 0.5 * 0.1
> in audition (out 0 (mce2 z (slew z 4000 4000)))

> let z = saw AR 800 * 0.1
> in audition (out 0 (mce2 z (slew z 400 400)))

Drawings

> import Sound.SC3.Plot

> let z = lfPulse AR 800 0 0.5
> plot_ugen1 0.1 z
> plot_ugen1 0.1 (slew z 4000 4000)
> plot_ugen1 0.1 (slew z 500 500)

