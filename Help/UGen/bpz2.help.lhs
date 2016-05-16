> Sound.SC3.UGen.Help.viewSC3Help "BPZ2"
> Sound.SC3.UGen.DB.ugenSummary "BPZ2"

> import Sound.SC3

> let n = whiteNoise 'α' AR
> in audition (out 0 (bpz2 (n * 0.25)))
