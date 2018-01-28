    Sound.SC3.UGen.Help.viewSC3Help "GaussTrig"
    Sound.SC3.UGen.DB.ugenSummary "GaussTrig"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let x = mouseX KR 0 0.9 Linear 0.2
>       t = gaussTrig KR 10 x * 0.5
>       n = whiteNoise 'α' AR * decay t 0.02 * 0.2
>   in fold2 (ringz n 2000 0.02) 0.2
