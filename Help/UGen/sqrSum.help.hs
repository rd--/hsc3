import Sound.SC3 {- hsc3 -}

g_01 = (fSinOsc AR 800 0 `sqrSum` fSinOsc AR (xLine KR 200 500 5 DoNothing) 0) * 0.125
