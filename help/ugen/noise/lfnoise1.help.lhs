lfnoise1 id freq

Ramp noise.  Generates linearly interpolated random values at a
rate given by the nearest integer division of the sample rate by
the freq argument.

freq - approximate rate at which to generate random values.

> lfnoise1 0 AR 1000 * 0.25

Modulate frequency.

> lfnoise1 0 AR (xline KR 1000 10000 10 2) * 0.25

Use as frequency control.

> sinosc AR (lfnoise1 0 KR 4 * 400 + 450) 0 * 0.2
