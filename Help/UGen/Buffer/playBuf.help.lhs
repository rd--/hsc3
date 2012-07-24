> Sound.SC3.UGen.Help.viewSC3Help "PlayBuf"
> Sound.SC3.UGen.DB.ugenSummary "PlayBuf"

> import Sound.SC3

Load sound file to buffer zero (single channel file required for examples)
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

Play once only.
> let s = bufRateScale KR 0
> in audition (out 0 (playBuf 1 AR 0 s 1 0 NoLoop RemoveSynth))

Play in infinite loop.
> let s = bufRateScale KR 0
> in audition (out 0 (playBuf 1 AR 0 s 1 0 Loop DoNothing))

Trigger playback at each pulse.
> let {t = impulse KR 2 0
>     ;s = bufRateScale KR 0}
> in audition (out 0 (playBuf 1 AR 0 s t 0 NoLoop DoNothing))

Trigger playback at each pulse (diminishing intervals).
> let {f = xLine KR 0.1 100 10 RemoveSynth
>     ;t = impulse KR f 0
>     ;s = bufRateScale KR 0}
> in audition (out 0 (playBuf 1 AR 0 s t 0 NoLoop DoNothing))

Loop playback, accelerating pitch.
> let r = xLine KR 0.1 100 60 RemoveSynth
> in audition (out 0 (playBuf 1 AR 0 r 1 0 Loop DoNothing))

Sine wave control of playback rate, negative rate plays backwards.
> let {f = xLine KR 0.2 8 30 RemoveSynth
>     ;r = fSinOsc KR f 0 * 3 + 0.6
>     ;s = bufRateScale KR 0 * r}
> in audition (out 0 (playBuf 1 AR 0 s 1 0 Loop DoNothing))

Release buffer.
> withSC3 (\fd -> send fd (b_free 0))
