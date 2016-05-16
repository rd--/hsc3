> Sound.SC3.UGen.Help.viewSC3Help "RLPF"
> Sound.SC3.UGen.DB.ugenSummary "RLPF"

> import Sound.SC3

> let {n = whiteNoise 'α' AR
>     ;f = sinOsc AR 0.5 0 * 40 + 220
>     ;r = rlpf n f 0.1}
> in audition (out 0 r)

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> in audition (out 0 (rlpf (saw AR 200 * 0.1) f 0.2))
