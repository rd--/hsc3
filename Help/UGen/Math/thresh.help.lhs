thresh a b

Signal thresholding.  0 when a < b, otherwise a.

> n <- lfNoise0 AR 50
> audition (out 0 (thresh (n * 0.5) 0.45))
