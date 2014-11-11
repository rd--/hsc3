> Sound.SC3.UGen.Help.viewSC3Help "LocalOut"
> Sound.SC3.UGen.DB.ugenSummary "LocalOut"

> import Sound.SC3

Resonator, must subtract blockSize for correct tuning

> let {p = localIn 1 AR 0
>     ;i = impulse AR 1 0
>     ;d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)}
> in audition (mrg [offsetOut 0 p,localOut d])

Compare with oscillator.

> audition (out 1 (sinOsc AR 440 0 * 0.2))

ping pong

> let {n = decay (impulse AR 0.3 0) 0.1 * whiteNoise 'α' AR * 0.2
>     ;l = localIn 2 AR 0 + mce2 n 0
>     ;d = delayN l 0.2 0.2
>     ;o = localOut (mceReverse d * 0.8)}
> in audition (mrg2 (out 0 d) o)

alternately, same graph

> let n = decay (impulse AR 0.3 0) 0.1 * whiteNoise 'α' AR * 0.2
> let l = localIn 2 AR 0 + mce2 n 0
> let d = delayN l 0.2 0.2
> let o = localOut (mceReverse d * 0.8)
> audition (mrg2 (out 0 d) o)
