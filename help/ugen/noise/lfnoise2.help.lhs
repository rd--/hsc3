lfnoise2 id freq

Quadratic noise.  Generates quadratically interpolated random
values at a rate given by the nearest integer division of the
sample rate by the freq argument.

> lfnoise2 0 AR 1000 * 0.25

Modulate frequency.

> lfnoise2 0 AR (xline KR 1000 10000 10 2) * 0.25

Use as frequency control.

> sinosc AR (lfnoise2 0 KR 4 * 400 + 450) 0 * 0.2
