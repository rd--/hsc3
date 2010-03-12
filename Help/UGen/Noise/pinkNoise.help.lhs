pinkNoise rate

Generates noise whose spectrum falls off in power by 3 dB per
octave.  This gives equal power over the span of each octave.  This
version gives 8 octaves of pink noise.

> import Sound.SC3.Monadic

> audition . (out 0) . (* 0.05) =<< pinkNoise AR

> audition . (out 0) . (* 0.05) =<< whiteNoise AR

> audition . (out 0) . (* 0.05) =<< brownNoise AR
