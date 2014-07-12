> Sound.SC3.UGen.Help.viewSC3Help "SetResetFF"
> Sound.SC3.UGen.DB.ugenSummary "SetResetFF"

> import Sound.SC3.ID

d0 is the set trigger, d1 the reset trigger

> let {n = brownNoise 'α' AR
>     ;d0 = dust 'β' AR 5
>     ;d1 = dust 'γ' AR 5}
> in audition (out 0 (setResetFF d0 d1 * n * 0.2))

silence

> let tr = setResetFF (impulse KR 5 0) (impulse KR 10 0)
> in audition (out 0 (brownNoise 'α' AR * 0.1 * decay2 tr 0.01 0.05))

duty cycle

> let tr = 1 - setResetFF (impulse KR 10 0) (impulse KR 5 0)
> in audition (out 0 (brownNoise 'α' AR * 0.1 * decay2 tr 0.01 0.05))
