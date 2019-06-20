> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>     let z = soundIn 0
>         d = mouseX KR 0.990 0.999 Linear 0.2
>         c = X.envFollow KR z d
>         o = pinkNoise 'α' AR * c
>     in mce2 z o
