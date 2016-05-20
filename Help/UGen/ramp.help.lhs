    > Sound.SC3.UGen.Help.viewSC3Help "Ramp"
    > Sound.SC3.UGen.DB.ugenSummary "Ramp"

> import Sound.SC3 {- hsc3 -}

Used to lag pitch

> g_01 =
>     let o = lfPulse KR 4 0 0.5 * 50 + 400
>         l = line KR 0 1 15 DoNothing
>         f = ramp o l
>     in sinOsc AR f 0 * 0.3

mouse control

> g_02 =
>     let x = mouseX KR 220 440 Exponential 0
>         x' = ramp x (300 / 1000)
>     in sinOsc AR (mce2 x x') 0 * 0.1
