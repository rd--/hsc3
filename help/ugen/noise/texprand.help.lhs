texprand id lo hi trig

Generates a random float value in exponential distribution from lo
to hi each time the trig signal changes from nonpositive to
positive values lo and hi must both have the same sign and be
non-zero.

> sinosc AR (texprand 0 KR 300.0 3000.0 (dust 0 KR 10)) 0 * 0.2
