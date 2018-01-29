    Sound.SC3.UGen.Help.viewSC3Help "GaussTrig"
    Sound.SC3.UGen.DB.ugenSummary "GaussTrig"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let x = mouseX KR 0 0.9 Linear 0.2
>       t1 = gaussTrig KR 10 x * abs (whiteNoise 'α' KR) * 0.5
>       t2 = dust 'β' KR 10 * 0.5
>       n = pinkNoise 'γ' AR * decay (mce2 t1 t2) 0.02 * 0.5
>   in fold2 (ringz n 2000 0.02) 0.5
