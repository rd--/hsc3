> Sound.SC3.UGen.Help.viewSC3Help "Ringz"
> Sound.SC3.UGen.DB.ugenSummary "Ringz"

> import Sound.SC3.ID

> let n = dust 'a' AR 3
> in audition (out 0 (ringz (n * 0.3) 2000 2))

> let n = whiteNoise 'a' AR
> in audition (out 0 (ringz (n * 0.005) 2000 0.5))

Modulate frequency
> let {n = whiteNoise 'a' AR
>     ;f = xLine KR 100 3000 10 RemoveSynth}
> in audition (out 0 (ringz (n * 0.005) f 0.5))

> let f = xLine KR 100 3000 10 RemoveSynth
> in audition (out 0 (ringz (impulse AR 6 0.3) f 0.5))

Modulate ring time
> let rt = xLine KR 4 0.04 8 RemoveSynth
> in audition (out 0 (ringz (impulse AR 6 0.3) 2000 rt))

Modulate ring time opposite direction
> let rt = xLine KR 0.04 4 8 RemoveSynth
> in audition (out 0 (ringz (impulse AR 6 0.3) 2000 rt))
