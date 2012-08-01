> Sound.SC3.UGen.Help.viewSC3Help "COsc"
> Sound.SC3.UGen.DB.ugenSummary "COsc"

> import Sound.SC3

Allocate and fill buffer.
> let {f = [Normalise,Wavetable,Clear]
>     ;d = [1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10]}
> in withSC3 ( do {_ <- async (b_alloc 10 512 1)
>                 ;async (b_gen_sine1 10 f d)})

Fixed beat frequency
> audition (out 0 (cOsc AR 10 200 0.7 * 0.1))

Modulate beat frequency with mouseX
> audition (out 0 (cOsc AR 10 200 (mouseX KR 0 4 Linear 0.2) * 0.1))

Compare with plain osc
> audition (out 0 (osc AR 10 200 0.0 * 0.1))
