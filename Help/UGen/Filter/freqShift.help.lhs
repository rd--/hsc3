freqShift input shift phase

freq-shift implements single sideband amplitude modulation, also
known as frequency shifting, but not to be confused with pitch
shifting.  Frequency shifting moves all the components of a signal
by a fixed amount but does not preserve the original harmonic
relationships.

input - audio input
shift - amount of shift in cycles per second
phase - phase of the frequency shift (0 - 2pi) 

shifting a 100Hz tone by 1 Hz rising to 500Hz

> let { i = sinOsc AR 100 0
>     ; s = xLine KR 1 500 5 RemoveSynth }
> in audition (out 0 (freqShift i s 0 * 0.1))

shifting a complex tone by 1 Hz rising to 500Hz

> let { d = klangSpec [101, 303, 606, 808] [1, 1, 1, 1] [1, 1, 1, 1]
>     ; i = klang AR 1 0 d
>     ; s = xLine KR 1 500 5 RemoveSynth }
> in audition (out 0 (freqShift i s 0 * 0.1))

modulating shift and phase

> do { s <- lfNoise2 AR 0.3
>    ; let { i = sinOsc AR 10 0
>          ; p = linLin (sinOsc AR 500 0) (-1) 1 0 (2 * pi) }
>      in audition (out 0 (freqShift i (s * 1500) p * 0.1)) }

shifting bandpassed noise

> do { n1 <- whiteNoise AR
>    ; n2 <- lfNoise0 AR 5.5
>    ; let { i = bpf n1 1000 0.001
>          ; s = n2 * 1000 }
>      in audition (out 0 (freqShift i s 0 * 32)) }
