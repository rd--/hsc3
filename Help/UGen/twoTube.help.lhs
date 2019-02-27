    Sound.SC3.UGen.Help.viewSC3Help "TwoTube"
    Sound.SC3.UGen.DB.ugenSummary "TwoTube"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let dly1 = 100
>       dly2 = 40
>       env = envelope [1,1,0] [(dly1 + dly2) / sampleRate,0.0] [EnvLin]
>       x = mouseX KR (-1) 1 Linear 0.2
>       y = mouseY KR 1 4 Linear 0.2
>       src = whiteNoise 'α' AR * 0.5 * envGen AR (impulse KR y 0) 1 0 1 DoNothing env
>   in X.twoTube AR src x 0.99 dly1 dly2
