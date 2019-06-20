> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let x = mouseX KR 20 12000 Exponential 0.2
>       y = mouseY KR 0.01 1 Linear 0.2
>       o = lfSaw AR (mce [x * 0.99,x * 1.01]) 0 * 0.1
>       freq = sinOsc KR (sinOsc KR 0.1 0) (1.5 * pi) * 1550 + 1800
>       s = X.iirFilter o freq y
>   in combN s 0.5 (mce2 0.4 0.35) 2 * 0.4 + s * 0.5
