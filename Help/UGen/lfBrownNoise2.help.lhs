    > Sound.SC3.UGen.Help.viewSC3Help "LFBrownNoise2"
    > Sound.SC3.UGen.DB.ugenSummary "LFBrownNoise2"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins as E {- hsc3 -}

Modulate frequency.

> g_01 =
>     let x = mouseX KR 0 5 Linear 0.2
>     in E.lfBrownNoise2 'α' AR 1000 1 x * 0.25

Use as frequency control.

> g_02 =
>     let f = E.lfBrownNoise2 'α' KR 8 0.2 0 * 400 + 450
>     in sinOsc AR f 0 * 0.2
