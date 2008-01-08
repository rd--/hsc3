lag2 in lagTime

Lag2 is the same as lag KR (lag KR s t) t.

> let x = mouseX KR 220 440 Exponential 0.1
> audition (out 0 (sinOsc AR (mce [x, lag2 x 1]) 0 * 0.1))
