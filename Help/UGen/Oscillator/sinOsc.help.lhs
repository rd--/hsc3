> Sound.SC3.UGen.Help.viewSC3Help "SinOsc"
> Sound.SC3.UGen.DB.ugenSummary "SinOsc"

> import Sound.SC3

Fixed frequency

> audition (out 0 (sinOsc AR 440 0 * 0.25))

Modulate freq

> audition (out 0 (sinOsc AR (xLine KR 2000 200 9 RemoveSynth) 0 * 0.5))

Modulate freq

> let f = sinOsc AR (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Modulate phase

> let p = sinOsc AR (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
> in audition (out 0 (sinOsc AR 800 p * 0.1))

Simple bell-like tone.

> let {f = mce [0.5,1,1.19,1.56,2,2.51,2.66,3.01,4.1]
>     ;a = mce [0.25,1,0.8,0.5,0.9,0.4,0.3,0.6,0.1]
>     ;o = sinOsc AR (500 * f) 0 * a
>     ;e = envGen KR 1 0.1 0 1 RemoveSynth (envPerc 0.01 10)}
> in audition (out 0 (mix o * e))

"When two pure tones of slightly different frequency are superposed,
our ears perceive audible beats at a rate given by the difference of
the two frequencies."

> let {f0 = 220;f1 = 221.25;d = abs (f1 - f0)}
> in audition (out 0 (sinOsc AR (mce2 f0 f1) 0 * 0.1 + impulse AR d 0 * 0.25))

"When two tones are sounded together, a tone of lower frequency is
frequently heard. Such a tone is called a combination tone.  The most
commonly heard combination tone occurs at a frequency f2 - f1."

> let {f1 = 300
>     ;f2 = 300 * (3/2)
>     ;f = mce2 (mce2 f1 f2) (abs (f2 - f1))
>     ;a = mce2 0.1 (max (sinOsc KR 0.05 0 * 0.1) 0)}
> in audition (out 0 (sinOsc AR f 0 * a))
