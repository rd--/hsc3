> Sound.SC3.UGen.Help.viewSC3Help "TGaussRand"
> Sound.SC3.UGen.DB.ugenSummary "TGaussRand"

> import Sound.SC3.ID

> let {t = dust 'α' KR 10
>     ;f = tGaussRand 'β' 300 3000 t
>     ;o = sinOsc AR f 0
>     ;l = tGaussRand 'γ' (-1) 1 t
>     ;p = pan2 o l 0.1}
> in audition (out 0 p)

compare to tRand
> let {t = dust 'α' KR 10
>     ;f = tRand 'β' 300 3000 t
>     ;o = sinOsc AR f 0
>     ;l = tRand 'γ' (-1) 1 t
>     ;p = pan2 o l 0.1}
> in audition (out 0 p)
