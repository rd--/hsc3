> Sound.SC3.UGen.Help.viewSC3Help "Phasor"
> Sound.SC3.UGen.DB.ugenSummary "Phasor"

> import Sound.SC3

phasor controls sine frequency, end frequency matches second sine.
> let {rate = mouseX KR 0.2 2 Exponential 0.1
>     ;tr = impulse AR rate 0
>     ;sr = sampleRate
>     ;x = phasor AR tr (rate / sr) 0 1 0
>     ;f = mce [linLin x 0 1 600 1000, 1000]}
> in audition (out 0 (sinOsc AR f 0 * 0.2))
