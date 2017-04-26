    Sound.SC3.UGen.Help.viewSC3Help "Env.*perc"
    :t envPerc'

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let a = 0.1
>         p = envPerc 0.01 1
>         e = envGen KR 1 a 0 1 RemoveSynth p
>     in sinOsc AR 440 0 * e

> g_02 =
>     let a = 0.1
>         c = EnvNum (-4)
>         p = envPerc' 0.01 1 a (c,c)
>         e = envGen KR 1 1 0 1 RemoveSynth p
>     in sinOsc AR 440 0 * e


import Sound.SC3.Plot {- hsc3-plot -}
plotEnvelope [envPerc 0.05 1,envPerc 0.2 0.75]
