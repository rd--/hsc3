> Sound.SC3.UGen.Help.viewSC3Help "Operator.clip2"
> :t clip2

> import Sound.SC3

clipping distortion
> audition (out 0 (clip2 (fSinOsc AR 400 0) 0.2))

> let l = line KR 0 1 8 RemoveSynth
> in audition (out 0 (clip2 (fSinOsc AR 400 0) l))
