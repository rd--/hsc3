twoZero in freq radius

Two zero filter

> import Sound.SC3.Monadic

> do { n <- whiteNoise AR
>    ; let f = xLine KR 20 20000 8 RemoveSynth
>      in audition (out 0 (twoZero (n * 0.125) f 1)) }
