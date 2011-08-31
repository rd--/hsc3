> Sound.SC3.UGen.Help.viewSC3Help "Resonz"
> Sound.SC3.UGen.DB.ugenSummary "Resonz"

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (resonz (n * 0.5) 2000 0.1))

Modulate frequency
> let { n = whiteNoise 'a' AR
>     ; f = xLine KR 1000 8000 10 RemoveSynth }
> in audition (out 0 (resonz (n * 0.5) f 0.05))

Modulate bandwidth
> let { n = whiteNoise 'a' AR
>     ; bw = xLine KR 1 0.001 8 RemoveSynth }
> in audition (out 0 (resonz (n * 0.5) 2000 bw))

Modulate bandwidth opposite direction
> let { n = whiteNoise 'a' AR
>     ; bw = xLine KR 0.001 1 8 RemoveSynth }
> in audition (out 0 (resonz (n * 0.5) 2000 bw))
