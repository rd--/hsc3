> Sound.SC3.UGen.Help.viewSC3Help "MidEQ"
> Sound.SC3.UGen.DB.ugenSummary "MidEQ"

> import Sound.SC3

> let f = midiCPS (fSinOsc KR 1 0 * 24 + 84)
> in audition (out 0 (midEQ (saw AR 200 * 0.2) f 0.3 12))

> let {i = pinkNoise 'α' AR * 0.2 + sinOsc AR 600 0 * 0.1
>     ;f = sinOsc KR 0.2 (0.5 * pi) * 2 + 600}
> in audition (out 0 (midEQ i f 0.01 (-24)))
