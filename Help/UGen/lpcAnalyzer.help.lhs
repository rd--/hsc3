    > Sound.SC3.UGen.Help.viewSC3Help "LPCAnalyzer"
    > Sound.SC3.UGen.DB.ugenSummary "LPCAnalyzer"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 = X.lpcAnalyzer (soundIn 0) (impulse AR 440 0 * 0.2) 256 50 0 0.999 0

> g_02 = X.lpcAnalyzer (soundIn 0) (impulse AR 440 0 * 0.2) 256 50 0 0.999 1

> g_03 =
>   let x = mouseX KR 1 128 Linear 0.2
>   in X.lpcAnalyzer (soundIn 0) (impulse AR 440 0 * 0.2) 128 x 0 0.999 0

> g_04 =
>   let x = mouseX KR 1 128 Linear 0.2
>   in X.lpcAnalyzer (soundIn 0) (impulse AR 440 0 * 0.2) 1024 x 0 0.999 1

> g_05 =
>   let x = mouseX KR 1 256 Linear 0.2
>   in X.lpcAnalyzer (soundIn 0) (whiteNoise 'α' AR * 0.1) 256 x 0 0.999 0
