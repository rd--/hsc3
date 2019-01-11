    Sound.SC3.UGen.Help.viewSC3Help "RedPhasor"
    Sound.SC3.UGen.DB.ugenSummary "RedPhasor"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.F0 {- hsc3 -}

no looping & it will play through once. Mouse x acts as trigger

> g_01 =
>   let tr = mouseX KR 0 1 Linear 0.2 >** 0.5
>   in sinOsc AR (redPhasor KR tr 0.3 400 800 0 500 600) 0 * 0.2

mouse y controls looping on/off, mouse x trigger

> g_02 =
>   let tr = mouseX KR 0 1 Linear 0.2 >** 0.5
>       lp = mouseY KR 0 1 Linear 0.2 >** 0.5
>   in sinOsc AR (redPhasor KR tr 0.3 400 800 lp 500 600) 0 * 0.2

mouse x controls loop rate, mouse y scales the start looppoint

> g_03 =
>   let x = mouseX KR 0 5 Linear 0.2
>       y = mouseY KR 200 500 Linear 0.2
>   in sinOsc AR (redPhasor KR 0 x 400 800 1 y 600) 0 * 0.2
