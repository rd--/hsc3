envASR :: UGen -> UGen -> UGen -> EnvCurve -> [UGen]

Attack, sustain, release envelope.  Arguments are:

    aT = attackTime (0.01)
    sL = sustainLevel (1)
    rT = releaseTime (1)
    c = curve (-4)

> import Sound.SC3

> let { g = control KR "gate" 1
>     ; p = envASR 0.01 1 1 (EnvNum (-4))
>     ; e = envGen KR g 0.1 0 1 RemoveSynth p }
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 0))
