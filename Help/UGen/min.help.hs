import Sound.SC3 {- hsc3 -}

g_01 = (fSinOsc AR 500 0 `min` fSinOsc AR 0.1 0) * 0.1
