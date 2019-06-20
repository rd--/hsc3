> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

default values

> g_01 =
>   let (i1,i2) = (soundIn 0,soundIn 1)
>   in X.jPverbRaw i1 i2 0 0.707 2000 1 500 1 0.1 2 1 1 1

