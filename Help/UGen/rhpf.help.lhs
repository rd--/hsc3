> Sound.SC3.UGen.Help.viewSC3Help "RHPF"
> Sound.SC3.UGen.DB.ugenSummary "RHPF"

> import Sound.SC3

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> in audition (out 0 (rhpf (saw AR 200 * 0.1) f 0.2))
