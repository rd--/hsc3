> Sound.SC3.UGen.Help.viewSC3Help "LFNoise1"
> Sound.SC3.UGen.DB.ugenSummary "LFNoise1"

> import Sound.SC3.ID

> audition (out 0 (lfNoise1 'α' AR 1000 * 0.05))

Modulate frequency.

> let {f = xLine KR 1000 10000 10 RemoveSynth
>     ;n = lfNoise1 'α' AR f}
> in audition (out 0 (n * 0.05))

Use as frequency control.

> let {n = lfNoise1 'α' KR 4
>     ;f = n * 400 + 450}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
