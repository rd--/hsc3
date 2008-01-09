oneZero in coef

One zero filter

> do { n <- whiteNoise AR
>    ; audition (out 0 (oneZero (n * 0.5) 0.5)) }

> do { n <- whiteNoise AR
>    ; audition (out 0 (oneZero (n * 0.5) (-0.5))) }

> do { n <- whiteNoise AR
>    ; let c = line KR (-0.5) 0.5 10 RemoveSynth
>      in audition (out 0 (oneZero (n * 0.5) c)) }
