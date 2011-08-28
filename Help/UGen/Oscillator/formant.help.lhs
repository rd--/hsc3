> Sound.SC3.UGen.Help.viewSC3Help "Formant"
> Sound.SC3.UGen.DB.ugenSummary "Formant"

> import Sound.SC3

Modulate fundamental frequency, formant frequency stays constant.
> let f = xLine KR 400 1000 8 RemoveSynth
> in audition (out 0 (formant AR f 2000 800 * 0.125))

Modulate formant frequency, fundamental frequency stays constant.
> let {f = mce [200, 300, 400, 500]
>     ;ff = xLine KR 400 4000 8 RemoveSynth}
> in audition (out 0 (formant AR f ff 200 * 0.125))

Modulate width frequency, other frequencies stay constant.
> let bw = xLine KR 800 8000 8 RemoveSynth
> in audition (out 0 (formant AR 400 2000 bw * 0.1))
