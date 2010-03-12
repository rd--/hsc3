thresh a b

Signal thresholding.  0 when a < b, otherwise a.

> import Sound.SC3.ID

> let n = lfNoise0 'a' AR 50
> in audition (out 0 (thresh (n * 0.5) 0.45))
