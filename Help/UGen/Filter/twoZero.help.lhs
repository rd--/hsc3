twoZero in freq radius

Two zero filter

> n <- whiteNoise AR
> audition $ twoZero (n * 0.125) (xLine KR 20 20000 8 RemoveSynth) 1
