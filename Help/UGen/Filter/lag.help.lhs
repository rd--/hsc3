lag in lagTime

A simple averaging filter.

> let x = mouseX KR 220 440 Linear 0.2
> audition $ sinOsc AR (MCE [x, lag x 1]) 0 * 0.1
