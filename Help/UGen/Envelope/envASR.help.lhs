> Sound.SC3.UGen.Help.viewSC3Help "Env.*asr"
> :i Sound.SC3.ASR

> import Sound.SC3

> let {g = control KR "gate" 1
>     ;p = envASR 0.01 1 1 (EnvNum (-4))
>     ;e = envGen KR g 0.1 0 1 RemoveSynth p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (send (n_set1 (-1) "gate" 0))

> import Sound.SC3.Plot

> plotEnvelope [envASR 0.1 1 1 (EnvNum (-4))
>              ,envASR 0.3 0.25 1 EnvSin
>              ,envASR 0.01 0.5 1.25 EnvLin]
