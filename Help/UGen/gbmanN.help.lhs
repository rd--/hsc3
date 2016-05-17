    Sound.SC3.UGen.Help.viewSC3Help "GbmanN"
    Sound.SC3.UGen.DB.ugenSummary "GbmanN"

> import Sound.SC3 {- hsc3 -}

default initial params

> g_01 =
>     let x = mouseX KR 20 sampleRate Linear 0.2
>     in gbmanN AR x 1.2 2.1 * 0.1

change initial params

> g_02 =
>     let x = mouseX KR 20 sampleRate Linear 0.2
>     in gbmanN AR x (-0.7) (-2.7) * 0.1

wait for it...

> g_03 =
>     let x = mouseX KR 20 sampleRate Linear 0.2
>     in gbmanN AR x 1.2 2.0002 * 0.1

as a frequency control

> g_04 =
>     let f = gbmanN AR 40 1.2 2.1 * 400 + 500
>     in sinOsc AR f 0 * 0.4
