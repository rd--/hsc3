    Sound.SC3.UGen.Help.viewSC3Help "DoubleWell3"
    Sound.SC3.UGen.DB.ugenSummary "DoubleWell3"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

bass synth

> g_01 =
>   let x = mouseX KR 0 200 Linear 0.2
>       y = mouseY KR 0.5 4.0 Linear 0.2
>       f = sinOsc AR x 0 * y
>   in X.doubleWell3 AR 0 0.01 f 0.25 0 0

gradually changing

> g_02 =
>   let f = lfSaw AR (line KR 10 1000 10 DoNothing) 0
>       delta = line KR 0.0 0.3 20 DoNothing
>   in X.doubleWell3 AR 0 0.05 f delta 0 0
