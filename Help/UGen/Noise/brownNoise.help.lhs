brownNoise rate

Generates noise whose spectrum falls off in power by 6 dB per
octave.

> do { n <- brownNoise AR
>    ; audition (out 0 (n * 0.1)) }

> audition . (out 0) . (* 0.1) =<< whiteNoise AR

> do { n <- brownNoise KR
>    ; let o = sinOsc AR (linExp n (-1) 1 64 9600) 0 * 0.1
>      in audition (out 0 o) }
