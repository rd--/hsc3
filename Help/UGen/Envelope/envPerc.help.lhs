> Sound.SC3.UGen.Help.viewSC3Help "Env.*perc"
> :t envPerc'

> import Sound.SC3

> let {a = 0.1
>     ;p = envPerc 0.01 1
>     ;e = envGen KR 1 a 0 1 RemoveSynth p }
> in audition (out 0 (sinOsc AR 440 0 * e))

> let {a = 0.1
>     ;c = EnvNum (-4)
>     ;p = envPerc' 0.01 1 a (c,c)
>     ;e = envGen KR 1 1 0 1 RemoveSynth p }
> in audition (out 0 (sinOsc AR 440 0 * e))
