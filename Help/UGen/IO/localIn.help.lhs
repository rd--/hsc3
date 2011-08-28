> Sound.SC3.UGen.Help.viewSC3Help "LocalIn"
> Sound.SC3.UGen.DB.ugenSummary "LocalIn"

> import Sound.SC3.ID

Ping-pong delay
> let {n = whiteNoise 'a' AR
>     ;a0 = decay (impulse AR 0.3 0) 0.1 * n * 0.2
>     ;a1 = localIn 2 AR + mce [a0,0]
>     ;a2 = delayN a1 0.2 0.2
>     ;a3 = mceEdit reverse a2 * 0.8}
> in audition (mrg [localOut a3,out 0 a2])
