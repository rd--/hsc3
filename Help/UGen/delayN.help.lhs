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

Flanging

> f_03 lp_f =
>   let f = 0.1 -- flanger freq
>       g = 0.1 -- feedback
>       i = soundIn (mce2 0 1) -- two channels of input signal
>       fb = i + localIn 2 AR 0 -- add feedback
>       e = delayN fb 0.02 (sinOsc KR f 0 * 0.005 + 0.005) -- max delay of 20msec
>       o = localOut (lp_f e * g)
>   in mrg2 (i + e) o

> g_03 = f_03 id
> g_04 = f_03 (\x -> bpf x (mouseX KR 1000 10000 Linear 0.2) 0.1) -- filter in the feedback loop
