> Sound.SC3.UGen.Help.viewSC3Help "MouseY"
> Sound.SC3.UGen.DB.ugenSummary "MouseY"

> import Sound.SC3

Frequency at X axis and amplitude at Y axis.

> let {freq = mouseX KR 20 2000 Exponential 0.1
>     ;ampl = mouseY KR 0.01 0.1 Linear 0.1}
> in audition (out 0 (sinOsc AR freq 0 * ampl))

There is a variant with equal arguments but a random traversal.

> let {freq = mouseX' KR 20 2000 Exponential 0.1
>     ;ampl = mouseY' KR 0.01 0.1 Linear 0.1}
> in audition (out 0 (sinOsc AR freq 0 * ampl))
