brownNoise rate

Generates noise whose spectrum falls off in power by 6 dB per
octave.

> n <- brownNoise AR
> audition (out 0 (n * 0.1))

> audition . (out 0) . (* 0.1) =<< whiteNoise AR

> n <- brownNoise KR
> audition (out 0 (sinOsc AR (linExp n (-1) 1 64 9600) 0 * 0.1))
