gate in trig

The signal at `in' is passed while `trig' is greater than zero.

> let t = lfPulse AR 1 0 0.1
> audition $ gate (fSinOsc AR 500 0 * 0.25) t
