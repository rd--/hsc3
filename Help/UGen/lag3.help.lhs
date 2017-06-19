     Sound.SC3.UGen.Help.viewSC3Help "Lag3"
    Sound.SC3.UGen.DB.ugenSummary "Lag3"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR 220 440 Exponential 0.1
>     in sinOsc AR (mce [x, lag3 x 1]) 0 * 0.1
