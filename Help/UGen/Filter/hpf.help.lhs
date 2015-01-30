> Sound.SC3.UGen.Help.viewSC3Help "HPF"
> Sound.SC3.UGen.DB.ugenSummary "HPF"

> import Sound.SC3

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> in audition (out 0 (hpf (saw AR 200 * 0.2) f))
