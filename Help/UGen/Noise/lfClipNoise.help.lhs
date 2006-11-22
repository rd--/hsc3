lfclipnoise id freq

Randomly generates the values -1 or +1 at a rate given by the
nearest integer division of the sample rate by the freq argument.
It is probably pretty hard on your speakers.  The freq argument is
the approximate rate at which to generate random values.

> lfclipnoise 0 AR 1000 * 0.25

Modulate frequency

> lfclipnoise 0 AR (xline KR 1000 10000 10 2) * 0.25

Use as frequency control

> sinosc AR (lfclipnoise 0 KR 4 * 200 + 600) 0 * 0.2
