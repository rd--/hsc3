> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let i = soundIn 0
>       c = X.coyote KR i 0.2 0.2 0.01 0.5 0.05 0.1
>       o = pinkNoise 'α' AR * decay c 1
>   in mce2 i o
