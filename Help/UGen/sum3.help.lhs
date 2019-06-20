> import Sound.SC3 {- hsc3 -}

> g_01 = sum3 (sinOsc AR 440 0) (sinOsc AR 441 0) (sinOsc AR 442 0) * 0.1

> g_02 = (sinOsc AR 440 0 + sinOsc AR 441 0 + sinOsc AR 442 0) * 0.1

> g_03 = mix (sinOsc AR (mce [440 .. 442]) 0) * 0.1
