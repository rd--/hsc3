inRange in lo hi

Tests if a signal is within a given range.

If in is >= lo and <= hi output 1.0, otherwise output 0.0. Output
is initially zero.

in - signal to be tested
lo - low threshold
hi - high threshold

> import Sound.SC3.ID

> let { n = brownNoise 'α' AR
>     ; x = mouseX KR 1 2 Linear 0.1 
>     ; o = sinOsc KR x 0 * 0.2 }
> in audition (out 0 (inRange o (-0.15) 0.15 * n * 0.1))
