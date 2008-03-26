fm-iter (rd)

> let { t0 = impulse AR (recip 0.30) 0
>     ; t1 = tDelay t0 0.15
>     ; t = mce2 t0 t1 }
> in do { k <- tRand 56 57 t
>       ; i <- tRand 40 480 t
>       ; j <- tRand (-1) 1 t
>       ; let { c = midiCPS k
>             ; m = midiCPS (k + 1 + j)
>             ; s = envPerc 0.01 0.9
>             ; e = envGen AR t 0.1 0 1 RemoveSynth s
>             ; f = sinOsc AR c 0 * i + m }
>         in audition (out 0 (sinOsc AR f 0 * e)) }
