> Sound.SC3.UGen.Help.viewSC3Help "max"
> :t max

> import Sound.SC3

q modulates and envelopes p
> let { p = fSinOsc AR 500 0 * 0.25
>     ; q = fSinOsc AR 0.5 0 }
> in audition (out 0 (p `max` q))
