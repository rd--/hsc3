    Sound.SC3.UGen.Help.viewSC3Help "Spreader"
    Sound.SC3.UGen.DB.ugenSummary "Spreader"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let theta = pi / 2
>       filtsPerOctave = 8
>   in X.spreader AR (pinkNoise 'α' AR * 0.1) theta filtsPerOctave

> g_02 =
>   let theta = mouseX KR 0 (pi / 2) Linear 0.2
>       filtsPerOctave = 8
>   in X.spreader AR (pinkNoise 'α' AR * 0.1) theta filtsPerOctave

> g_03 =
>   let in_ = sinOsc AR (mouseX KR 110 880 Exponential 0.2) 0 * 0.1
>       theta = pi / 2
>       filtsPerOctave = 8
>   in X.spreader AR in_ theta filtsPerOctave
