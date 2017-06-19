    > Sound.SC3.UGen.Help.viewSC3Help "Lag"
    > Sound.SC3.UGen.DB.ugenSummary "Lag"

> import Sound.SC3 {- hsc3 -}

used to lag pitch

> g_01 =
>     let x = mouseX KR 220 440 Linear 0.2
>     in sinOsc AR (mce [x, lag x 1]) 0 * 0.1

used to smooth amplitude changes

> g_02 =
>     let n = lfNoise0 'a' KR 0.5
>     in sinOsc AR (220 + (lag n 1 * 220)) 0 * (lag n 2 * 0.1)
