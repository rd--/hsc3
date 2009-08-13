envPerc :: UGen -> UGen -> [UGen]

Percussive envelope

> import Sound.SC3

> let { p = envPerc 0.01 1
>     ; e = envGen KR 1 0.1 0 1 RemoveSynth p }
> in audition (out 0 (sinOsc AR 440 0 * e))
