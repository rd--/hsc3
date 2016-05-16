    Sound.SC3.UGen.Help.viewSC3Help "MouseX"
    Sound.SC3.UGen.DB.ugenSummary "MouseX"

> import Sound.SC3

Frequency control

> g_01 =
>     let x = mouseX KR 40 10000 Exponential 0.2
>     in sinOsc AR x 0 * 0.1

There is a variant with equal arguments but random traversal.

> g_02 =
>     let x = mouseX' KR 40 10000 Exponential 0.2
>     in sinOsc AR x 0 * 0.1
