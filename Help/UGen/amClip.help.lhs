    Sound.SC3.UGen.Help.viewSC3Help "Operator.amclip"
    :t amClip

> import Sound.SC3 {- hsc3 -}

> g_01 = amClip (whiteNoise 'α' AR) (fSinOsc KR 1 0 * 0.2)
