> Sound.SC3.UGen.Help.viewSC3Help "Decay"
> Sound.SC3.UGen.DB.ugenSummary "Decay"

> import Sound.SC3.ID

Used as an envelope.
> let {n = pinkNoise 'a' AR
>     ;s = impulse AR (xLine KR 1 50 20 RemoveSynth) 0.25}
> in audition (out 0 (decay s 0.2 * n))
