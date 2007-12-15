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

(define rand-phase
  (lambda (m p _)
    (list m (Rand 0 3.14))))

(define noise-phase
  (lambda (m p _)
    (list m (range (LFNoise0 kr 3) 0 3.14))))

(define noise-mag
  (lambda (m p _)
    (list (Mul (GT (LFNoise0 kr 10) 0) m) p)))

> withSC3 (\fd -> do let async p m = send p m >> wait p "/done"
>                    async fd (b_alloc 10 1024 1)
>                    async fd (b_allocRead 11 "/home/rohan/audio/metal.wav" 0 0))
> let no_op m p _ = (m, p)
>     combf m p i = ((modE i 7.0 ==* 0) * m, p)
>     spectral_delay m p _ = (m + delayN m 1 v, p)
>         where v = linLin (lfPar KR 0.5 0) (-1) 1 0.1 1
>     bpf_sweep nf m p i = ((e <* 10) * m, p)
>         where e = abs (i - (linLin (lfPar KR 0.1 0) (-1) 1 2 (nf / 20)))
>     nf = 1024
>     sf = playBuf 1 11 (bufRateScale KR 11) 1 0 Loop
>     c1 = fft' 10 sf
>     c2 = pvcollect c1 nf spectral_delay 0 250 0
> audition (out 0 (0.1 * ifft' c2))
