    Sound.SC3.UGen.Help.viewSC3Help "Linen"
    Sound.SC3.UGen.DB.ugenSummary "Linen"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let e = linen (impulse KR 2 0) 0.01 0.6 0.4 DoNothing
>     in e * sinOsc AR 440 0 * 0.1

> g_02 =
>     let x = mouseX KR (-1) 1 Linear 0.1
>         y = mouseY KR 0.1 0.5 Linear 0.1
>         e = linen x 1 y 1.0 DoNothing
>     in sinOsc AR 440 0 * e
