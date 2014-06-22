> Sound.SC3.UGen.Help.viewSC3Help "MouseX"
> Sound.SC3.UGen.DB.ugenSummary "MouseX"

> import Sound.SC3

Frequency control

> let x = mouseX KR 40 10000 Exponential 0.2
> in audition (out 0 (sinOsc AR x 0 * 0.1))

There is a variant with equal arguments but random traversal.

> let x = mouseX' KR 40 10000 Exponential 0.2
> in audition (out 0 (sinOsc AR x 0 * 0.1))
