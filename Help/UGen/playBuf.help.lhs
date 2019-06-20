> import Sound.SC3 {- hsc3 -}

Load sound file to buffer zero (single channel file required for examples)

> fn_01 = "/home/rohan/data/audio/pf-c5.aif"

> m_01 = (b_allocRead 0 fn_01 0 0)

    withSC3 (async m_01)

Play once only.

> f_01 b = playBuf 1 AR b (bufRateScale KR b) 1 0 NoLoop RemoveSynth

> g_01 = f_01 28 -- 0 28

Play in infinite loop.

> g_02 = playBuf 1 AR 0 (bufRateScale KR 0) 1 0 Loop DoNothing

Trigger playback at each pulse.

> g_03 =
>     let t = impulse KR 2 0
>         s = bufRateScale KR 0
>     in playBuf 1 AR 0 s t 0 NoLoop DoNothing

Trigger playback at each pulse (diminishing intervals).

> g_04 =
>     let f = xLine KR 0.1 100 10 RemoveSynth
>         t = impulse KR f 0
>         s = bufRateScale KR 0
>     in playBuf 1 AR 0 s t 0 NoLoop DoNothing

Loop playback, accelerating pitch.

> g_05 =
>     let r = xLine KR 0.1 100 60 RemoveSynth
>     in playBuf 1 AR 0 r 1 0 Loop DoNothing

Sine wave control of playback rate, negative rate plays backwards.

> g_06 =
>     let f = xLine KR 0.2 8 30 RemoveSynth
>         r = fSinOsc KR f 0 * 3 + 0.6
>         s = bufRateScale KR 0 * r
>     in playBuf 1 AR 0 s 1 0 Loop DoNothing

Channel mismatch, single channel buffer, two channel playBuf, result
is single channel playback and channel mismatch message in server log.

> g_07 = playBuf 2 AR 0 (bufRateScale KR 0) 1 0 Loop DoNothing

Graph will play both channels after loading a two channel signal to
buffer.

> f_02 = "/home/rohan/data/audio/sp/tinguely.aif"

> m_02 = b_allocRead 0 f_02 0 0

    withSC3 (async m_02)

Release buffer.

    withSC3 (send (b_free 0))

Scan sequence of buffers:

> f_08 n =
>     let t = impulse KR 2 0
>         b = mouseX KR 0 n Linear 0.2
>         r = bufRateScale KR b
>     in playBuf 1 AR b r t 0 Loop DoNothing

> g_08 = f_08 52 -- (29 * 6)

In sclanguage:

{var fn = "/home/rohan/data/audio/pf-c5.aif"
;s.sendMsg("/b_allocRead",0,fn,0,0)}.value

{var sc = BufRateScale.kr(0)
;Out.ar(0,PlayBuf.ar(2,0,sc,1,0,1,0))}.play

{var fn = "/home/rohan/data/audio/sp/tinguely.aif"
;s.sendMsg("/b_allocRead",0,fn,0,0)}.value

{var b = MouseX.kr(32,64,0,0.2)
;var r = BufRateScale.kr(b)
;Out.ar(0,PlayBuf.ar(1,b,r,1,0,1,0))}.play
