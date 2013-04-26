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

Self-modulation at control rate.
> let {fr = blip KR 0.25 3 * 300 + 500
>     ;nh = blip KR 0.15 2 * 20 + 21}
> in audition (out 0 (blip AR fr nh * 0.2))
