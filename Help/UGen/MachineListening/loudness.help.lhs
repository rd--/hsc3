> Sound.SC3.UGen.Help.viewSC3Help "Loudness"
> Sound.SC3.UGen.DB.ugenSummary "Loudness"

> import Sound.SC3

Assume hop of half fftsize
> withSC3 (async (b_alloc 10 1024 1))

> let { x = mouseX' KR 0.001 0.1 Exponential 0.2
>     ; i = sinOsc AR 1000 0 * x
>     ; f = fft' 10 i
>     ; l = loudness f 0.25 6
>     ; o = sinOsc AR (mce2 900 (l * 300 + 600)) 0 * 0.1 }
> in audition (out 0 o)
