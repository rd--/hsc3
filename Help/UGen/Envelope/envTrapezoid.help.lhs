> :t envTrapezoid

> import Sound.SC3

> let { t = envTrapezoid 0.05 0.95 3 0.1
>     ; e = envGen KR 1 1 0 1 RemoveSynth t }
> in audition (out 0 (sinOsc AR 440 0 * e))

> let e = [0,3,-1,-1,0.1,0.5,1,0,0.1,0,1,0,0,1.5,1,0]
> in envTrapezoid 0 0.25 2 0.1 == e
