brownNoise

Generates noise whose spectrum falls off in power by 6 dB per
octave.

> n <- brownNoise AR
> audition (n * 0.1)

> audition . (* 0.1) =<< whiteNoise AR
