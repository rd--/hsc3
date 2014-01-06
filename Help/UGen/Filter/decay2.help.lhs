> Sound.SC3.UGen.Help.viewSC3Help "Decay2"
> Sound.SC3.UGen.DB.ugenSummary "Decay2"

> import Sound.SC3

Used as an envelope

> let {s = fSinOsc AR 600 0 * 0.25
>     ;f = xLine KR 1 50 20 RemoveSynth}
> in audition (out 0 (decay2 (impulse AR f 0) 0.01 0.2 * s))

Compare the above with Decay used as the envelope.

> let {s = fSinOsc AR 600 0 * 0.25
>     ;f = xLine KR 1 50 20 RemoveSynth}
> in audition (out 0 (decay (impulse AR f 0) 0.2 * s))
