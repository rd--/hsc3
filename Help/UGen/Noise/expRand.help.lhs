expRand lo hi

Generates a single random float value in an exponential
distributions from `lo' to `hi'.

> let a = line KR 0.5 0 0.01 RemoveSynth
> f <- expRand 100.0 8000.0
> audition (out 0 (fSinOsc AR f 0 * a))
