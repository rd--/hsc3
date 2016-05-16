> Sound.SC3.UGen.Help.viewSC3Help "FMGrainB"
> Sound.SC3.UGen.DB.ugenSummary "FMGrainB"

> import Sound.SC3

> withSC3 (do {_ <- async (b_alloc 10 512 1)
>             ;let f = [Normalise,Wavetable,Clear]
>              in send (b_gen_sine2 10 f [(0.5,0.1)])})

> let {t = impulse AR 20 0
>     ;n = linLin (lfNoise1 'α' KR 1) (-1) 1 1 10
>     ;s = envSine 9 0.1
>     ;e = envGen KR 1 1 0 1 RemoveSynth s
>     ;o = fmGrainB t 0.2 440 220 n 10 * e}
> in audition (out 0 o)
