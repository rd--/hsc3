dswitch index array

Demand rate generator for embedding different inputs

array - array of values or other ugens
index - which of the inputs to return

In difference to dswitch1, dswitch embeds all items of 
an input demand ugen first before looking up the next index.

> import Sound.SC3

> do { a0 <- dwhite 2 3 4
>    ; a1 <- dwhite 2 0 1
>    ; a2 <- dseq 2 (mce [1, 1, 1, 0])
>    ; i <- dseq 2 (mce [0, 1, 2, 1, 0])
>    ; d <- dswitch i (mce [a0, a1, a2])
>    ; let { t = impulse KR 4 0
>          ; f = demand t 0 d * 300 + 400
>          ; o = sinOsc AR f 0 * 0.1 }
>      in audition (out 0 o) }

compare with dswitch1

> do { a0 <- dwhite 2 3 4
>    ; a1 <- dwhite 2 0 1
>    ; a2 <- dseq 2 (mce [1, 1, 1, 0])
>    ; i <- dseq 2 (mce [0, 1, 2, 1, 0])
>    ; d <- dswitch1 i (mce [a0, a1, a2])
>    ; let { t = impulse KR 4 0
>          ; f = demand t 0 d * 300 + 400
>          ; o = sinOsc AR f 0 * 0.1 }
>      in audition (out 0 o) }

