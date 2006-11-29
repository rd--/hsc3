peak trig reset

Outputs the maximum value read at the `trig' input until `reset' is
triggered.

> t <- dust AR 20
> let r = impulse AR 0.4 0
> audition $ sinOsc AR (peak t r * 500 + 200) 0 * 0.2
