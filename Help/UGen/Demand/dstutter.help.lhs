dstutter n in

Demand rate input replicator.

n   - number of repeats (can be a demand ugen)
in  - input ugen

> import Sound.SC3

> do { inp <- dseq dinf (mce [1, 2, 3])
>    ; nse <- diwhite dinf 2 8
>    ; rep <- dstutter nse inp
>    ; let { trg = impulse KR (mouseX KR 1 40 Exponential 0.2) 0
>          ; frq = demand trg 0 rep * 30 + 340 }
>      in audition (out 0 (sinOsc AR frq 0 * 0.1)) }
