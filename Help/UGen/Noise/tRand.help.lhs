tRand lo hi trig

Generates a random float value in uniform distribution from lo each
time the trig signal changes from nonpositive to positive values

> import Sound.SC3.ID

> let { t = dust 'a' KR (mce [5, 12])
>     ; f = tRand 'b' (mce [200, 1600]) (mce [500, 3000]) t }
> in audition (out 0 (sinOsc AR f 0 * 0.2))
