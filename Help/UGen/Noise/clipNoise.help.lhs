clipNoise rate

Generates noise whose values are either -1 or 1.  This produces the
maximum energy for the least peak to peak amplitude.

> audition . (out 0) . (* 0.1) =<< clipNoise AR

> audition . (out 0) . (* 0.1) =<< whiteNoise AR
