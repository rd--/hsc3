klink

> do { n1 <- lfNoise0 KR (mce2 0.5 1.5)
>    ; let { o = sinOsc KR n1 0
>          ; f = mce2 2 3
>          ; a = abs (slope o) * f
>          ; t = impulse AR a 0
>          ; i = decay2 t 0.01 0.1
>          ; x = mouseX KR 960 3620 Exponential 0.2
>          ; y = mouseY KR 0.5 2.0 Linear 0.2 }
>      in do { n2 <- tRand x 3940 t
>            ; n3 <- tRand 0.005 0.275 t
>            ; audition (out 0 (ringz i n2 (n3 * y))) } }
