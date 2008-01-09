klang rate freqScale freqOffset spec

Bank of fixed oscillators.  spec is constructed using klangSpec, which
takes lists of frequency, amplitude and phase.

> let { f = [440,550..1100]
>     ; a = take 7 (cycle [0.05, 0.02])
>     ; p = replicate 7 0 }
> in audition (out 0 (klang AR 1 0 (klangSpec f a p)))
