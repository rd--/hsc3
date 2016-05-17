    Sound.SC3.UGen.Help.viewSC3Help "CuspL"
    Sound.SC3.UGen.DB.ugenSummary "CuspL"

> import Sound.SC3 {- hsc3 -}

Vary frequency

> g_01 =
>     let x = mouseX KR 20 sampleRate Linear 0.1
>     in cuspL AR x 1.0 1.99 0 * 0.3

Mouse-controlled parameters.

> g_02 =
>     let x = mouseX KR 0.9 1.1 Linear 0.1
>         y = mouseY KR 1.8 2.0 Linear 0.1
>     in cuspL AR (sampleRate / 4) x y 0 * 0.3

As frequency control.

> g_03 =
>     let x = mouseX KR 0.9 1.1 Linear 0.1
>         y = mouseY KR 1.8 2.0 Linear 0.1
>         n = cuspL AR 40 x y 0 * 0.3
>     in sinOsc AR (n * 800 + 900) 0 * 0.4
