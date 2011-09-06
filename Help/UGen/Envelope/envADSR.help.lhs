> Sound.SC3.UGen.Help.viewSC3Help "Env.adsr"
> :t envADSR

> import Sound.SC3

> let {g = control KR "gate" 1
>     ;p = envADSR 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
>     ;e = envGen KR g 0.1 0 1 DoNothing p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 0))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 1))

> withSC3 (\fd -> send fd (n_free [-1]))
