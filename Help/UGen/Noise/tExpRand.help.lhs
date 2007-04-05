tExpRand lo hi trig

Generates a random float value in exponential distribution from lo
to hi each time the trig signal changes from nonpositive to
positive values lo and hi must both have the same sign and be
non-zero.

> t <- dust KR 10
> f <- tExpRand 300.0 3000.0 t
> audition (out 0 (sinOsc AR f 0 * 0.2))
