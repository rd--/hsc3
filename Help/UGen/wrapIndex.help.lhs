> Sound.SC3.UGen.Help.viewSC3Help "WrapIndex"
> Sound.SC3.UGen.DB.ugenSummary "WrapIndex"

> import Sound.SC3

> withSC3 (async (b_alloc_setn1 0 0 [200,300,400,500,600,800]))

> let {x = mouseX KR 0 18 Linear 0.1
>     ;f = wrapIndex 0 x}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

> let {b = asLocalBuf 'α' [200,300,400,500,600,800]
>     ;x = mouseX KR 0 18 Linear 0.1
>     ;f = wrapIndex b x}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
