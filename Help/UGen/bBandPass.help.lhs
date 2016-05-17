    Sound.SC3.UGen.Help.viewSC3Help "BBandPass"
    Sound.SC3.UGen.DB.ugenSummary "BBandPass"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let i = soundIn 4
>         f = mouseX KR 20 20000 Exponential 0.2
>         bw = mouseY KR 0 10 Linear 0.2
>     in bBandPass i f bw
