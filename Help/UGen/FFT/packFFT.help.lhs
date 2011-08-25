packFFT chain bufsize frombin tobin zeroothers magsphases

Pack separate demand-rate FFT bin streams into an FFT chain buffer

Takes a length-prefixed array of magnitudes and phases, and packs
them into an FFT buffer ready for transforming back into
time-domain audio using IFFT.

Most people won't need to use this directly - instead, use
pvcollect, pvcalc, or pvcalc2.

The input data is magsphases, which should be a flat array
containing magnitude and phase of all bins in ascending order.
e.g. [mag0, phase0, mag1, phase1, mag2, phase2, ... magN, phaseN]
This input is typically demand-rate.

This is technically similar to Demand or Duty in that it calls
demand-rate UGens further up the graph to process the values,
eventually calling UnpackFFT. These two ends of the process must in
most cases see the same chain...! Otherwise behaviour is undefined
and, who knows, possibly unpleasant.

frombin and tobin allow you to fill the supplied data only into a
subset of the FFT bins (i.e. a single delimited frequency band),
set zeroothers to 1 to zero all the magnitudes outside this band
(otherwise they stay intact).

For usage examples, see UnpackFFT, but also pvcollect, pvcalc,
pvcalc2.

Here's an unusual example which uses PackFFT without using
UnpackFFT first - essentially creating our FFT data from scratch.

> import Control.Monad
> import Sound.SC3.Monadic

> withSC3 (\fd -> send fd (b_alloc 10 512 1))

> let {n = 100
>     ;range u l r = linLin u (-1) 1 l r
>     ;square a = a * a
>     ;r1 = do {f <- expRand 0.1 1
>              ;return (range (fSinOsc KR f 0) 0 1)}}
> in do {m1 <- replicateM n r1
>       ;let {m2 = zipWith (*) m1 (map square [1.0, 0.99 ..])
>            ;r2 = do {r <- iRand (-3) 5
>                     ;return (lfPulse KR (2 ** r) 0 0.3)}}
>        in do {i <- replicateM n r2
>              ;let {m3 = zipWith (*) m2 i
>                   ;p = replicate n 0.0
>                   ;c1 = fft' 10 (fSinOsc AR 440 0)
>                   ;ci = constant . fromIntegral
>                   ;c2 = packFFT c1 512 0 (ci n - 1) 1 (packFFTSpec m3 p)
>                   ;s = ifft' c2}
>               in audition (out 0 (mce [s, s]))}}
