> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 = softClipAmp4 AR (sinOsc AR 220 0 * 0.1) (mouseX KR 1 16 Linear 0.2)

> g_02 = softClipAmp4 AR (soundIn 0) (mouseX KR 1 8 Linear 0.2)

