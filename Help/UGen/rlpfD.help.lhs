> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let s = mix (lfSaw AR (mce2 120 180) 0 * 0.33)
>       f = linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 280 1500
>   in X.rlpfD s f 0.6 0.5 * 3
