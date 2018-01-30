    Sound.SC3.UGen.Help.viewSC3Help "Lag"
    Sound.SC3.UGen.DB.ugenSummary "Lag"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR 220 440 Linear 0.2
>     in sinOsc AR (mce [x, lag x 1]) 0 * 0.1

> g_02 =
>     let n = lfNoise0 'α' KR 0.5
>     in sinOsc AR (220 + (lag n 1 * 220)) 0 * (lag n 2 * 0.1)

> g_03 = lag (impulse AR 100 0) (mouseX KR 0.0 0.01 Linear 0.2)

> g_04 = lag (lfPulse AR 50 0 0.5) (mouseX KR 0.0 (1/50) Linear 0.2) * 0.2
