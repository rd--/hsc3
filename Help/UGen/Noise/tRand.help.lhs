tRand lo hi trig

Generates a random float value in uniform distribution from lo each
time the trig signal changes from nonpositive to positive values

> do { t <- dust KR (mce [5, 12])
>    ; f <- tRand (mce [200, 1600]) (mce [500, 3000]) t
>    ; audition (out 0 (sinOsc AR f 0 * 0.2)) }
