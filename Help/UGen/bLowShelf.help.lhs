    Sound.SC3.UGen.Help.viewSC3Help "BLowShelf"
    Sound.SC3.UGen.DB.ugenSummary "BLowShelf"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let i = soundIn (mce2 0 1)
>         freq = mouseX KR 40 6000 Exponential 0.2
>         rs = 1
>         db = mouseY KR 24 (-24) Linear 0.2
>     in bLowShelf i freq rs db

> g_02 =
>     let i = soundIn (mce2 0 1)
>         freq = mouseX KR 20 6000 Exponential 0.2
>         rs = mouseY KR 0.1 1 Linear 0.2
>         db = 6
>     in bLowShelf i freq rs db
