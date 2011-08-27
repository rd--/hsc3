pvcollect chain numframes func frombin tobin zeroothers

Process each bin of an FFT chain separately.

pvcollect applies function func to each bin of an FFT chain. func
should be a function that takes magnitude, phase, index as inputs
and returns a resulting [magnitude, phase].

The "index" is the integer bin number, starting at 0 for DC. You
can optionally ignore the phase and only return a single
(magnitude) value, in which case the phase is assumed to be left
unchanged.

frombin, tobin, and zeroothers are optional arguments which limit
the processing to a specified integer range of FFT bins. If
zeroothers is set to 1 then bins outside of the range being
processed are silenced.

Note that this procedure can be relatively CPU-heavy, depending on
how you use it.

> import Sound.SC3

> let fileName = "/home/rohan/audio/metal.wav"
> in withSC3 (\fd -> do { async fd (b_alloc 10 1024 1)
>                       ; async fd (b_allocRead 11 fileName 0 0) })

> let { no_op m p _ = (m, p)
>     ; combf m p i = ((fmod i 7.0 ==* 0) * m, p)
>     ; spectral_delay m p _ = let { l = lfPar KR 0.5 0
>                                  ; v = linLin l (-1) 1 0.1 1 }
>                              in (m + delayN m 1 v, p)
>     ; nf = 1024
>     ; bpf_sweep m p i = let { l = lfPar KR 0.1 0
>                             ; e = abs (i - (linLin l (-1) 1 2 (nf / 20))) }
>                         in ((e <* 10) * m, p)
>     ; sf = playBuf 1 11 (bufRateScale KR 11) 1 0 Loop DoNothing
>     ; c1 = fft' 10 sf
>     ; c2 = pvcollect c1 nf spectral_delay 0 250 0 }
> in audition (out 0 (0.1 * ifft' c2))
