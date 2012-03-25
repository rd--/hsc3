> Sound.SC3.UGen.Help.viewSC3Help "Tartini"
> Sound.SC3.UGen.DB.ugenSummary "Tartini"

> import Sound.SC3.ID

Comparison of input frequency (x) and tracked oscillator frequency (f).
> let {x = mouseX KR 440 880 Exponential 0.1
>     ;o = lfSaw AR x 0 * 0.05 {- sinOsc AR x 0 * 0.1 -}
>     ;MCE [f,e] = tartini KR o 0.2 2048 0 1024 0.5
>     ;t = impulse KR 4 0
>     ;pf = poll t f (label "f") 0
>     ;px = poll t x (label "x") 0}
> in audition (mrg [out 0 o,pf,px])

Fast test of live pitch tracking, not careful with amplitude of input
(see better example below)
> let MCE [f,e] = tartini KR (soundIn 0) 0.2 2048 0 1024 0.5
> in audition (out 0 (saw AR f * 0.05))
