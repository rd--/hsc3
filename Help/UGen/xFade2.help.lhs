    > Sound.SC3.UGen.Help.viewSC3Help "XFade2"
    > Sound.SC3.UGen.DB.ugenSummary "XFade2"

> import Sound.SC3 {- hsc3 -}

> g_01 = xFade2 (saw AR 440) (sinOsc AR 440 0) (lfTri KR 0.1 0) 0.1
