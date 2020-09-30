> import Sound.SC3 {- hsc3 -}

> e_01 = map from_warp [Linear,Exponential]

    e_01 == [0,1]

Frequency control

> g_01 =
>     let x = mouseX KR 40 10000 Exponential 0.2
>     in sinOsc AR x 0 * 0.1

There is a variant with equal arguments but random traversal

> g_02 =
>     let x = mouseX' KR 40 10000 Exponential 0.2
>     in sinOsc AR x 0 * 0.1
