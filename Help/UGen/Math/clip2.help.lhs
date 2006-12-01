clip2 a b

Bilateral clipping.  Clips a to +/- b

> audition $ clip2 (fSinOsc AR 400 0) 0.2

> audition $ clip2 (fSinOsc AR 400 0) (line KR 0 1 8 RemoveSynth)
