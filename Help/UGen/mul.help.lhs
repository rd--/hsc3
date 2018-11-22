    Sound.SC3.UGen.Help.viewSC3Help "Operator.*"
    :t (*)

> import Sound.SC3 {- hsc3 -}

> g_01 = sinOsc AR 440 0 * 0.15

Creates a beating effect (subaudio rate).

> g_02 = fSinOsc KR 10 0 * pinkNoise 'α' AR * 0.5

Ring modulation.

> g_03 =
>     let p = sinOsc AR (xLine KR 100 1001 10 DoNothing) 0
>         q = syncSaw AR 100 200
>     in p * q * 0.25
