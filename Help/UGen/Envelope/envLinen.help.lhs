> Sound.SC3.UGen.Help.viewSC3Help "Env.*linen"
> :i Sound.SC3.LINEN
> :t envLinen

> import Sound.SC3

> let {t = envLinen 0.4 2 0.4 0.1
>     ;e = envGen KR 1 1 0 1 RemoveSynth t}
> in audition (out 0 (sinOsc AR 440 0 * e))

> import Sound.SC3.Plot

> plotEnvelope [envLinen 0.4 2 0.4 1
>              ,envLinen 0.6 1 1.2 0.6]
