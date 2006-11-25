pinkNoise

Generates noise whose spectrum falls off in power by 3 dB per
octave.  This gives equal power over the span of each octave.  This
version gives 8 octaves of pink noise.

> audition . (* 0.15) =<< pinkNoise AR

> audition . (* 0.15) =<< whiteNoise AR

> audition . (* 0.15) =<< brownNoise AR
