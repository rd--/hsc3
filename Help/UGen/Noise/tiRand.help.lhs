tiRand lo hi trig

Generates a random integer value in uniform distribution from lo to
hi each time the trig signal changes from nonpositive to positive
values

> l <- tiRand (-1) 1 =<< dust KR 10
> n <- pinkNoise AR
> audition (out 0 (pan2 (n * 0.1) l 1))

> f <- tiRand 4 12 =<< dust KR 10
> audition (out 0 (sinOsc AR (f * 150 + (MCE [0,1])) 0 * 0.1))
