> import Sound.SC3
> :t envCoord

Co-ordinate (break-point) envelope

> let {c = EnvLin
>     ;p = envCoord [(0,0),(0.5,0.1),(0.55,1),(1,0)] 9 0.1 c
>     ;e = envGen KR 1 1 0 1 RemoveSynth p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> import Sound.SC3.Plot

> let {c0 = [(0,0),(0.35,0.1),(0.55,1),(1,0)]
>     ;c1 = [(0,0),(0.15,0.6),(0.35,0.2),(1,0)]}
> in plotEnvelope [envCoord c0 9 0.1 EnvLin,envCoord c1 6 0.1 EnvLin]

