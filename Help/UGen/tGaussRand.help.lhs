> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> f_01 rand_f =
>     let t = dust 'α' KR 10
>         f = rand_f 'β' 300 3000 t
>         o = sinOsc AR f 0
>         l = rand_f 'γ' (-1) 1 t
>     in pan2 o l 0.1

> g_01 = f_01 X.tGaussRand

compare to tRand

> g_02 = f_01 tRand
