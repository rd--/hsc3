> Sound.SC3.UGen.Help.viewSC3Help "Index"
> Sound.SC3.UGen.DB.ugenSummary "Index"

> import Sound.SC3

Allocate and set values at buffer ten
> withSC3 (async (b_alloc_setn1 10 0 [50,100,200,400,800,1600]))

Index buffer for frequency values
> let f = index 10 (lfSaw KR 2 3 * 4)
> in audition (out 0 (sinOsc AR (mce [f,f * 9]) 0 * 0.1))

Free buffer
> withSC3 (send (b_free 10))
