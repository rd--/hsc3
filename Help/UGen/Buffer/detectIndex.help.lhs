> Sound.SC3.UGen.Help.viewSC3Help "DetectIndex"
> Sound.SC3.UGen.DB.ugenSummary "DetectIndex"

> import Sound.SC3

Allocate and set values at buffer ten
> withSC3 (\fd -> async fd (b_alloc_setn1 10 0 [2,3,4,0,1,5]))

Find indexes and map to an audible frequency range.
> let {n = 6
>     ;x = floorE (mouseX' KR 0 n Linear 0.1)
>     ;i = detectIndex 10 x}
> in audition (out 0 (sinOsc AR (linExp i 0 n 200 700) 0 * 0.1))

Free buffer.
> withSC3 (\fd -> send fd (b_free 10))
