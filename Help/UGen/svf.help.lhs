    Sound.SC3.UGen.Help.viewSC3Help "SVF"
    Sound.SC3.UGen.DB.ugenSummary "SVF"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let signal = lfSaw AR (range 110 35 (lfSaw KR 2 0)) 0
>       cutoff = mouseX KR 20 20000 Exponential 0.2
>       res = mouseY KR 1 0 Linear 0.2
>       k = control KR
>       low = k "low" 0.1
>       band = k "band" 0.0
>       high = k "high" 0.0
>       notch = k "notch" 0.0
>       peak_ = k "peak" 0.0
>   in X.svf signal cutoff res low band high notch peak_

> f_01 k v = withSC3 (sendMessage (n_set1 (-1) k v))

    f_01 "low" 0.1
    f_01 "band" 0.0
    f_01 "high" 0.0
    f_01 "notch" 0.0
    f_01 "peak" 0.0
