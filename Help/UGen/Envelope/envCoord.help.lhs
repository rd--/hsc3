> import Sound.SC3
> :t envCoord

Co-ordinate (break-point) envelope

> let {c = EnvLin
>     ;p = envCoord [(0,0),(0.5,0.1),(0.55,1),(1,0)] 9 0.1 c
>     ;e = envGen KR 1 1 0 1 RemoveSynth p}
> in audition (out 0 (sinOsc AR 440 0 * e))

