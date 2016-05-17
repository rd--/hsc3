    Sound.SC3.UGen.Help.viewSC3Help "BHiPass"
    Sound.SC3.UGen.DB.ugenSummary "BHiPass"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let i = whiteNoise 'α' AR {- soundIn (mce2 0 1) -}
>         f = mouseX KR 10 20000 Exponential 0.2
>         rq = mouseY KR 0 1 Linear 0.2
>     in bHiPass i f rq
