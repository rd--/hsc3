> Sound.SC3.UGen.Help.viewSC3Help "LFBrownNoise2"
> Sound.SC3.UGen.DB.ugenSummary "LFBrownNoise2"

> import Sound.SC3.ID

Modulate frequency.
> let {x = mouseX KR 0 5 Linear 0.2
>     ;n = lfBrownNoise2 'α' AR 1000 1 x}
> in audition (out 0 (n * 0.25))

Use as frequency control.
> let f = lfBrownNoise2 'α' KR 8 0.2 0 * 400 + 450
> in audition (out 0 (sinOsc AR f 0 * 0.2))
