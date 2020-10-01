> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

default values

> g_01 =
>   let (i1,i2) = (soundIn 0,soundIn 1)
>   in X.jPverbRaw i1 i2 0 0.707 2000 1 500 1 0.1 2 1 1 1

dreamverb

> p_01 =
>   [("damp",0.31443298969072)
>   ,("earlydiff",0.421875)
>   ,("highband",1024.0219794048)
>   ,("highx",0.0)
>   ,("lowband",2450.0822520097)
>   ,("lowx",0.84375)
>   ,("mdepth",4.639175257732)
>   ,("mfreq",0.10309278350515)
>   ,("midx",0.70618556701031)
>   ,("size",2.7938144329897)
>   ,("t60",60)]

> g_02 =
>   let (i1,i2) = (soundIn 0,soundIn 1)
>       rvb = X.jPverbRaw i1 i2 0.314 0.421 1024.0219 0.0 2450.082 0.843 4.639 0.103 0.706 2.793 60.0
>   in mce2 i1 i2 + rvb

