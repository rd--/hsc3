    Sound.SC3.UGen.Help.viewSC3Help "BBandStop"
    Sound.SC3.UGen.DB.ugenSummary "BBandStop"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let i = soundIn (mce2 0 1)
>         f = mouseX KR 20 20000 Exponential 0.2
>         bw = mouseY KR 0 10 Linear 0.2
>     in bBandStop i f bw

> g_02 =
>     let i = sinOsc AR 1000 (mce2 0 0)
>         f = mouseX KR 800 1200 Exponential 0.2
>         bw = mouseY KR 0 10 Linear 0.2
>     in bBandStop i f bw
