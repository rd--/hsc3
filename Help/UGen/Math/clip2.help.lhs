clip2 a b

Bilateral clipping.  Clips a to +/- b

> import Sound.SC3

> audition (out 0 (clip2 (fSinOsc AR 400 0) 0.2))

> let l = line KR 0 1 8 RemoveSynth
> in audition (out 0 (clip2 (fSinOsc AR 400 0) l))
