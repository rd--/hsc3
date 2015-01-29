> Sound.SC3.UGen.Help.viewSC3Help "Free"
> Sound.SC3.UGen.DB.ugenSummary "Free"

> import Sound.SC3

> let {a = out 0 (sinOsc AR 880 0 * 0.1)
>     ;n0 = pinkNoise 'α' AR
>     ;n1 = dust 'β' KR 20
>     ;b = mrg [out 1 (n0 * 0.1), free n1 1001]}
> in withSC3 (do {_ <- async (d_recv (synthdef "a" a))
>                ;_ <- async (d_recv (synthdef "b" b))
>                ;send (s_new "a" 1001 AddToTail 1 [])
>                ;send (s_new "b" (-1) AddToTail 1 [])})
