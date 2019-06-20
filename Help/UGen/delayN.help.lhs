> import Sound.SC3 {- hsc3 -}

Dust randomly triggers Decay to create an exponential decay envelope
for the WhiteNoise input source.  The input is left, the delay right.

> g_01 =
>     let i = decay (dust 'α' AR 1) 0.3 * whiteNoise 'β' AR
>     in mce2 i (delayN i 0.1 0.1)

The delay time can be varied at control rate.  An oscillator either
reinforcing or cancelling with the delayed copy of itself.

> g_02 =
>     let i = sinOsc AR 320 0 * 0.1
>         maxdelaytime = 0.005
>         delaytime = mouseX KR 0.0 maxdelaytime Linear 0.15
>     in i + delayN i maxdelaytime delaytime
