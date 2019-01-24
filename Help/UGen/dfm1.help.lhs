    Sound.SC3.UGen.Help.viewSC3Help "DFM1"
    Sound.SC3.UGen.DB.ugenSummary "DFM1"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

Play it with the mouse

> g_01 =
>     let n = pinkNoise 'α' AR * 0.5
>         x = mouseX KR 80 5000 Exponential 0.1
>         y = mouseY KR 0.1 1.2 Linear 0.1
>     in X.dfm1 n x y 1 0 3e-4

Bass...

> g_02 =
>     let i = mix (pulse AR (mce2 100 100.1) 0.5) * 0.4
>         f = range 80 2000 (sinOsc KR (range 0.2 5 (sinOsc KR 0.3 0)) 0)
>     in X.dfm1 i f 1.1 2 0 0.0003 * 0.1
