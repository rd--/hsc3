    Sound.SC3.UGen.Help.viewSC3Help "BlitB3Saw"
    Sound.SC3.UGen.DB.ugenSummary "BlitB3Saw"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let f = xLine KR 1000 20 10 DoNothing
>     in blitB3Saw AR f 0.99 * 0.1

aliasing suddenly appears for very high frequencies

> g_02 =
>     let f = mouseX KR 10 10000 Exponential 0.2
>         c = mouseY KR 0.01 0.99 Linear 0.2
>     in blitB3Saw AR f c * 0.1

comparison

> g_03 = mce2 (saw AR 20) (blitB3Saw AR 20 0.99) * 0.1
