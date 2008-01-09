bpz2 in

Two zero fixed midpass.  This filter cuts out 0 Hz and the Nyquist
frequency.

> do { n <- whiteNoise AR
>    ; audition (out 0 (bpz2 (n * 0.25))) }
