pulseCount trig reset

This outputs the number of pulses received at `trig' and outputs
that value until `reset' is triggered.

> let c = pulseCount (impulse AR 10 0) (impulse AR 0.4 0)
> audition (out 0 (sinOsc AR (c * 200) 0 * 0.05))
