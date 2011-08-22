envLinen :: UGen -> UGen -> UGen -> UGen -> [UGen]

 aT = attack time
 sT = sustain time
 rT = release time
  l = level

Linear envelope parameter constructor.

> import Sound.SC3

> let { t = envLinen 0.4 2 0.4 0.1
>     ; e = envGen KR 1 1 0 1 RemoveSynth t }
> in audition (out 0 (sinOsc AR 440 0 * e))
