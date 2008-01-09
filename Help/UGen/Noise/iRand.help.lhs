iRand lo hi

Generates a single random integer value in uniform distribution
from `lo' to `hi'.

> do { f <- iRand 200 1200
>      ; let e = line KR 0.2 0 0.1 RemoveSynth
>        in audition (out 0 (fSinOsc AR f 0 * e)) }
