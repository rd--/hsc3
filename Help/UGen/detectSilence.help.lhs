    > Sound.SC3.UGen.Help.viewSC3Help "DetectSilence"
    > Sound.SC3.UGen.DB.ugenSummary "DetectSilence"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let s = sinOsc AR 440 0 * mouseY KR 0 0.2 Linear 0.1
>         d = detectSilence s 0.1 0.1 RemoveSynth
>     in mrg [s,d]
