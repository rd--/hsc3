    Sound.SC3.UGen.Help.viewSC3Help "Lag2"
    Sound.SC3.UGen.DB.ugenSummary "Lag2"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR 220 440 Exponential 0.1
>     in sinOsc AR (mce [x, lag2 x 1]) 0 * 0.1
