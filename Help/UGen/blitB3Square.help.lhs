    Sound.SC3.UGen.Help.viewSC3Help "BlitB3Square"
    Sound.SC3.UGen.DB.ugenSummary "BlitB3Square"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_00 =
>   let x = mouseX KR 20 400 Exponential 0.2
>   in X.blitB3Square AR x 0.99 * 0.1

> g_01 =
>     let f = xLine KR 1000 20 10 DoNothing
>     in X.blitB3Square AR f 0.99 * 0.1

aliasing suddenly appears for very high frequencies

> g_02 =
>     let f = mouseX KR 20 10000 Exponential 0.2
>         c = mouseY KR 0.001 0.999 Linear 0.2
>     in X.blitB3Square AR f c * 0.1

difference in CPU usage (excessive wire use,-w 1024)

> f_03 sqr_osc =
>     let f z = midiCPS (range 36 72 (lfNoise0 z KR (rand z 2 3)))
>         l z = rand z (-1) 1
>         o z = pan2 (sqr_osc AR (f z) * 0.1) (l z) 0.1
>     in sum (map o [0::Int .. 99])

> g_03 = f_03 (\rt f -> X.blitB3Square rt f 0.99)
> g_04 = f_03 (\rt f -> pulse rt f 0.5)
