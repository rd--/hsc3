    Sound.SC3.UGen.Help.viewSC3Help "Operator.ring4"
    :t ring4

> import Sound.SC3 {- hsc3 -}

> a = fSinOsc AR 800 0
> b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0
> g_01 = ring4 a b * 0.125

is equivalent to:

> g_02 = (((a * a * b) - (a * b * b))) * 0.125
