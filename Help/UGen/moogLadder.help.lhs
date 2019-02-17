    Sound.SC3.UGen.Help.viewSC3Help "MoogLadder"
    Sound.SC3.UGen.DB.ugenSummary "MoogLadder"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let o = mix (lfSaw AR (mce2 120 180) 0 * 0.33)
>       cf = linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 180 8500
>   in X.moogLadder o cf 0.75

> g_02 =
>   let n = dust 'α' AR 3
>   in X.moogLadder n 2000 (mouseY KR 0 1 Linear 0.2)

