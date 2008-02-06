dseq length array

Demand rate sequence generator.

array   - array of values or other ugens
length  - number of repeats

> do { n <- dseq 3 (mce [1, 3, 2, 7, 8])
>    ; let { x = mouseX KR 1 40 Exponential 0.1
>          ; t = impulse KR x 0
>          ; f = demand t 0 n * 30 + 340 }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }

At audio rate.

> do { n <- dseq dinf (mce [1,3,2,7,8,32,16,18,12,24])
>    ; let { x = mouseX KR 1 10000 Exponential 0.1
>          ; t = impulse AR x 0
>          ; f = demand t 0 n * 30 + 340 }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
