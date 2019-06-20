> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let n = X.lfBrownNoise0 'α' AR 10 0.05 0
>       f = linExp n (-1) 1 64 9600
>   in sinOsc AR f 0 * 0.1
