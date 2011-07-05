envADSR :: UGen->UGen->UGen->UGen->UGen->EnvCurve->UGen->[UGen]

Attack, decay, sustain, release envelope.  Arguments are:

    aT = attackTime (0.01)
    dT = decayTime (0.3)
    sL = sustainLevel (0.5, proportion of peakLevel)
    rT = releaseTime (1)
    pL = peakLevel (1)
    c = curve (-4)
    b = bias (0)

> import Sound.SC3

> let { g = control KR "gate" 1
>     ; p = envADSR 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
>     ; e = envGen KR g 0.1 0 1 DoNothing p }
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 0))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 1))

> withSC3 (\fd -> send fd (n_free [-1]))
