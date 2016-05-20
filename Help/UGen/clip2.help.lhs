    > Sound.SC3.UGen.Help.viewSC3Help "Operator.clip2"
    > :t clip2

> import Sound.SC3 {- hsc3 -}

clipping distortion

> g_01 = clip2 (fSinOsc AR 400 0) 0.2
>
> g_02 = clip2 (fSinOsc AR 400 0) (line KR 0 1 8 RemoveSynth)
