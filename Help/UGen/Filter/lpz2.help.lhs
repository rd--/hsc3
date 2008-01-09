lpz2 in

Two zero fixed lowpass filter

> do { n <- whiteNoise AR
>    ; audition (out 0 (lpz2 (n * 0.25))) }
