    > Sound.SC3.UGen.Help.viewSC3Help "LocalOut"
    > Sound.SC3.UGen.DB.ugenSummary "LocalOut"

> import Sound.SC3 {- hsc3 -}

Resonator, must subtract blockSize for correct tuning

> g_01 =
>     let p = localIn 1 AR 0
>         i = impulse AR 1 0
>         d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)
>     in mrg [offsetOut 0 p,localOut d]

Compare with oscillator.

> g_02 = sinOsc AR 440 0 * 0.2

ping pong

> g_03 =
>     let n = decay (impulse AR 0.3 0) 0.1 * whiteNoise 'α' AR * 0.2
>         l = localIn 2 AR 0 + mce2 n 0
>         d = delayN l 0.2 0.2
>         o = localOut (mceReverse d * 0.8)
>     in mrg2 d o
