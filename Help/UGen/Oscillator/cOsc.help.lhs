> Sound.SC3.UGen.Help.viewSC3Help "COsc"
> Sound.SC3.UGen.DB.ugenSummary "COsc"

> import Sound.SC3

Allocate and fill buffer.
> let d = [1+2+4,1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10]
> in withSC3 (\fd -> do {_ <- async fd (b_alloc 10 512 1)
>                       ;async fd (b_gen 10 "sine1" d)})

Fixed beat frequency
> audition (out 0 (cOsc AR 10 200 0.7 * 0.25))

Modulate beat frequency with mouseX
> audition (out 0 (cOsc AR 10 200 (mouseX' KR 0 4 Linear 0.2) * 0.25))

Compare with plain osc
> audition (out 0 (osc AR 10 200 0.0 * 0.25))
