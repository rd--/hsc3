lfTri rate freq iphase

A non-band-limited triangular waveform oscillator. Output ranges
from -1 to +1.

> import Sound.SC3

> audition (out 0 (lfTri AR 500 1 * 0.1))

Used as both Oscillator and LFO.

> audition (out 0 (lfTri AR (lfTri KR 4 0 * 400 + 400) 0 * 0.1))
