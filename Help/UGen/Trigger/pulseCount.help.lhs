pulseCount trig reset

This outputs the number of pulses received at `trig' and outputs
that value until `reset' is triggered.

> import Sound.SC3

> let c = pulseCount (impulse AR 10 0) (impulse AR 0.4 0)
> in audition (out 0 (sinOsc AR (c * 200) 0 * 0.05))
