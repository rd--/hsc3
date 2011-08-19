lag3 in lagTime

Lag3 is the same as lag (lag (lag s t) t) t.

> import Sound.SC3

> let x = mouseX' KR 220 440 Exponential 0.1
> in audition (out 0 (sinOsc AR (mce [x, lag3 x 1]) 0 * 0.1))
