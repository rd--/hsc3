> Sound.SC3.UGen.Help.viewSC3Help "Operator.amclip"
> :t amClip

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (amClip n (fSinOsc KR 1 0 * 0.2)))
