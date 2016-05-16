> Sound.SC3.UGen.Help.viewSC3Help "Pitch"
> Sound.SC3.UGen.DB.ugenSummary "Pitch"

> import Sound.SC3

> let {x = mouseX KR 220 660 Linear 0.1
>     ;y = mouseY KR 0.05 0.25 Linear 0.1
>     ;s = sinOsc AR x 0 * y
>     ;a = amplitude KR s 0.05 0.05
>     ;f = pitch s 440 60 4000 100 16 7 0.02 0.5 1 0}
> in audition (out 0 (mce [s, sinOsc AR (mceChannel 0 f / 2) 0 * a]))

> let {s = soundIn 4
>     ;a = amplitude KR s 0.1 0.1
>     ;f = pitch s 440 60 4000 100 16 7 0.02 0.5 1 0}
> in audition (out 0 (mce [s, sinOsc AR (mceChannel 0 f) 0 * a]))

Comparison of input frequency (x) and tracked oscillator frequency (f).
Output is printed to the console by scsynth.
> let {x = mouseX KR 440 880 Exponential 0.1
>     ;o = sinOsc AR x 0 * 0.1
>     ;[f,_] = mceChannels (pitch o 440 60 4000 100 16 7 0.02 0.5 1 0)
>     ;r = sinOsc AR f 0 * 0.1
>     ;t = impulse KR 4 0
>     ;pf = poll t f (label "f") 0
>     ;px = poll t x (label "x") 0}
> in audition (mrg [out 0 (mce2 o r),pf,px])
