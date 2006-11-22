rand id lo hi

Generates a single random value in uniform distribution from lo to
hi.  It generates this when the SynthDef first starts playing, and
remains fixed for the duration of the synth's existence.

> let s = fsinosc AR (rand 0 IR 200 1200) 0 * (line KR 0.2 0 0.1 2)
> in pan2 AR s (rand 0 IR (-1) 1) 1
