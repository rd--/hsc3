rand lo hi

Generates a single random value in uniform distribution from lo to
hi.  It generates this when the SynthDef first starts playing, and
remains fixed for the duration of the synth's existence.

> f <- rand 200 1200
> l <- rand (-1) 1
> let s = fSinOsc AR f 0 * (line KR 0.2 0 0.1 RemoveSynth)
> audition $ pan2 s l 1
