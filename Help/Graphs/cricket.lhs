cricket (rd)

> do { r1 <- clone 2 (rand 10 13)
>    ; r2 <- clone 2 (rand 10 13)
>    ; r3 <- clone 2 (rand 4 7)
>    ; let { t = impulse KR 0.7 0
>          ; e = decay2 (impulse KR r2 0) 0.001 0.005
>          ; f = sinOsc KR r1 0 * e * r3 }
>      in do { r4 <- clone 2 (tRand 2220 2227 t)
>            ; audition (out 0 (sinOsc AR r4 0 * f * 0.25)) } }

