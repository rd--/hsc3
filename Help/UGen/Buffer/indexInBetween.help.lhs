> Sound.SC3.UGen.Help.viewSC3Help "IndexInBetween"
> Sound.SC3.UGen.DB.ugenSummary "IndexInBetween"

> import Sound.SC3

Allocate and set values at buffer ten

> withSC3 (async (b_alloc_setn1 10 0 [200,210,400,430,600,800]))

Index into buffer for frequency values

> let {f0 = mouseX KR 200 900 Linear 0.1
>     ;i = indexInBetween 10 f0
>     ;l0 = index 10 i
>     ;l1 = index 10 (i + 1)
>     ;f1 = linLin (frac i) 0 1 l0 l1}
> in audition (out 0 (sinOsc AR (mce [f0,f1]) 0 * 0.1))

Free buffer

> withSC3 (send (b_free 10))
