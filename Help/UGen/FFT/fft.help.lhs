> Sound.SC3.UGen.Help.viewSC3Help "FFT"
> Sound.SC3.UGen.DB.ugenSummary "FFT"
> :t fft'

> import Sound.SC3.ID

> withSC3 (\fd -> async fd (b_alloc 10 2048 1))

> let n = whiteNoise 'a' AR
> in audition (out 0 (ifft' (fft' 10 (n * 0.05))))

> let { s0 = sinOsc KR 0.08 0 * 6 + 6.2
>     ; s1 = sinOsc KR (squared s0) 0 * 100 + 800
>     ; s2 = sinOsc AR s1 0 }
> in audition (out 0 (ifft' (fft' 10 s2) * 0.25))
