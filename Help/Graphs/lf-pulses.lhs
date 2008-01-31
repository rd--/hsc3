lf pulses (rd)

> do { n0 <- lfNoise0 AR (mce2 20 40)
>    ; n1 <- lfNoise0 AR (mce2 5 10)
>    ; let { x = mouseX KR 0.012 0.19 Exponential 0.1
>          ; f = formlet (blip AR 10 12) (n0 * 43 + 700) 0.005 x 
>          ; o = sinOsc AR 40 0 * n1 }
>      in audition (out 0 (clip2 (leakDC (f + o) 0.995) 0.75)) }
