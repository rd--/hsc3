> Sound.SC3.UGen.Help.viewSC3Help "LPF"
> Sound.SC3.UGen.DB.ugenSummary "LPF"

> import Sound.SC3

> let {f = xLine KR 0.7 300 20 RemoveSynth
>     ;ff = fSinOsc KR f 0 * 3600 + 4000}
> in audition (out 0 (lpf (saw AR 200 * 0.1) ff))

Control rate filtering.
> let ctl = lpf (lfPulse KR 8 0 0.5) (mouseX' KR 2 50 Exponential 0.1)
> in audition (out 0 (sinOsc AR (ctl * 200 + 400) 0 * 0.1))
