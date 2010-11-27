envADSR :: UGen->UGen->UGen->UGen->UGen->EnvCurve->UGen->[UGen]

Attack, decay, sustain, release envelope.  Argumets are:

    aT = attackTime (0.01)
    dT = decayTime (0.3)
    sL = sustainLevel (0.5)
    rT = releaseTime (1)
    pL = peakLevel (1)
    c = curve (-4)
    b = bias (0)

> import Sound.SC3

> let { g = control KR "gate" 1
>     ; p = envADSR 0.01 0.3 0.5 1 1 (EnvNum (-4)) 0
>     ; e = envGen KR g 0.1 0 1 RemoveSynth p }
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (\fd -> send fd (n_set1 (-1) "gate" 0))
