    Sound.SC3.UGen.Help.viewSC3Help "AudioMSG"
    Sound.SC3.UGen.DB.ugenSummary "AudioMSG"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 = audioMSG AR (sinOsc AR 220 0 * 0.1) (mouseX KR 0 (2 * pi) Linear 0.2)

> g_02 = audioMSG AR (soundIn 0) (mouseX KR 0 (2 * pi) Linear 0.2)

