    Sound.SC3.UGen.Help.viewSC3Help "CrossoverDistortion"
    Sound.SC3.UGen.DB.ugenSummary "CrossoverDistortion"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

{CrossoverDistortion.ar(SinOsc.ar([400, 404], 0, 0.2), MouseX.kr(0, 1), MouseY.kr(0, 1))}.play

> g_01 =
>   let x = mouseX KR 0 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>   in X.crossoverDistortion (sinOsc AR (mce2 400 404) 0 * 0.2) x y

{CrossoverDistortion.ar(SoundIn.ar, MouseX.kr(0, 1), MouseY.kr(0, 1))}.play

> g_02 =
>   let x = mouseX KR 0 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>   in X.crossoverDistortion (soundIn 0) x y
