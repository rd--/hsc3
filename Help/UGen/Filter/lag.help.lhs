lag in lagTime

A simple averaging filter.

> let x = mouseX KR 220 440 Linear 0.2
> in audition (out 0 (sinOsc AR (mce [x, lag x 1]) 0 * 0.1))
