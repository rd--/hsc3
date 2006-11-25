crackle chaosParam

A noise generator based on a chaotic function.  The parameter of
the chaotic function has useful values from just below 1.0 to just
above 2.0. Towards 2.0 the sound crackles.

The equation implemented is: y0 = fabs(y1 * param - y2 - 0.05f)

> audition $ crackle AR 1.95 * 0.2

Modulate chaos parameter

> audition $ crackle AR (line KR 1.0 2.0 3 RemoveSynth) * 0.2
