hpz2 in

Two zero fixed highpass filter.

> do { n <- whiteNoise AR
>    ; audition (out 0 (hpz2 (n * 0.25))) }
