> Sound.SC3.UGen.Help.viewSC3Help "Env.*adsr"
> :i ADSR
> :t envADSR_r
> :t envADSR

> import Sound.SC3

> let {g = control KR "gate" 1
>     ;p = envADSR 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
>     ;e = envGen KR g 0.1 0 1 DoNothing p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 0))
> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 1))
> withSC3 (\fd -> send fd (n_free [-1]))

There is a record variant:

> let {g = control KR "gate" 1
>     ;c = EnvNum (-4)
>     ;r = ADSR {attackTime = 0.75
>               ,decayTime = 0.75
>               ,sustainLevel = 0.5
>               ,releaseTime = 0.75
>               ,peakLevel = 1
>               ,curve = (c,c,c)
>               ,bias = 0}
>     ;p = envADSR_r r
>     ;e = envGen KR g 0.1 0 1 DoNothing p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 0))
> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 1))
> withSC3 (\fd -> send fd (n_free [-1]))
