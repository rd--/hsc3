iRand lo hi

Generates a single random integer value in uniform distribution
from `lo' to `hi'.

> f <- iRand 200 1200
> audition (out 0 (fSinOsc AR f 0 * (line KR 0.2 0 0.1 RemoveSynth)))
