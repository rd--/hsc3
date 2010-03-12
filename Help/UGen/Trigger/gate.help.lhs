gate in trig

The signal at `in' is passed while `trig' is greater than zero.

> import Sound.SC3

> let t = lfPulse AR 1 0 0.1
> in audition (out 0 (gate (fSinOsc AR 500 0 * 0.25) t))
