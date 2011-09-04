> Sound.SC3.UGen.Help.viewSC3Help "softclip"
> :t softClip

> import Sound.SC3

> let {e = xLine KR 0.1 10 10 RemoveSynth
>     ;o = fSinOsc AR 500 0.0}
> in audition (out 0 (softClip (o * e) * 0.25))
