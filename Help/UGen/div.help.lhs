    Sound.SC3.UGen.Help.viewSC3Help "Operator./"
    :t (/)

> import Sound.SC3 {- hsc3 -}

> g_01 = sinOsc AR 440 0 / 6

Creates a beating effect (subaudio rate).

> g_02 = (pinkNoise 'α' AR / fSinOsc KR 5 0) * 0.1

Optimises identity

> g_04 = sinOsc AR 440 0 / 1 * 0.1
