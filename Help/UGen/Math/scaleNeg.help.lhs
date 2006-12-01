scaleNeg a b

Scale negative part of input wave.  a * b when a < 0, otherwise a.

> audition $ scaleNeg (fSinOsc AR 500 0) (line AR 1 (-1) 4 RemoveSynth)
