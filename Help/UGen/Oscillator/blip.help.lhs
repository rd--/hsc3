> Sound.SC3.UGen.Help.viewSC3Help "Blip"
> Sound.SC3.UGen.DB.ugenSummary "Blip"

> import Sound.SC3

> audition (out 0 (blip AR 440 200 * 0.1))

Modulate frequency
> let f = xLine KR 20000 200 6 RemoveSynth
> in audition (out 0 (blip AR f 100 * 0.1))

Modulate number of harmonics.
> let nh = line KR 1 100 20 RemoveSynth
> in audition (out 0 (blip AR 200 nh * 0.2))
