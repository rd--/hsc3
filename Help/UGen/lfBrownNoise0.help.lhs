    Sound.SC3.UGen.Help.viewSC3Help "LFBrownNoise2"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let n = lfBrownNoise0 'α' AR 10 0.05 0
>       f = linExp n (-1) 1 64 9600
>   in sinOsc AR f 0 * 0.1
