     Sound.SC3.UGen.Help.viewSC3Help "Pitch"
     Sound.SC3.UGen.DB.ugenSummary "Pitch"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR 220 660 Linear 0.1
>         y = mouseY KR 0.05 0.25 Linear 0.1
>         s = sinOsc AR x 0 * y
>         a = amplitude KR s 0.05 0.05
>         f = pitch s 440 60 4000 100 16 7 0.02 0.5 1 0
>     in mce [s, sinOsc AR (mceChannel 0 f / 2) 0 * a]

Live input tracking, carelessly

> g_02 =
>     let s = hpf (soundIn 0) 90
>         a = lag (amplitude KR s 0.01 0.01) 0.2
>         [f,_] = mceChannels (pitch s 440 60 4000 100 16 1 0.02 0.5 1 0)
>         fq = midiCPS (roundE (lag (cpsMIDI f) 0.1))
>     in mce [s * 0.1, lfTri AR f 0 * lag a 0.2 * lag (f >** 90 * f <** 500) 0.2]

Comparison of input frequency (x) and tracked oscillator frequency (f).
Output is printed to the console by scsynth.

> g_03 =
>     let x = mouseX KR 440 880 Exponential 0.1
>         o = sinOsc AR x 0 * 0.1
>         [f,_] = mceChannels (pitch o 440 60 4000 100 16 7 0.02 0.5 1 0)
>         r = sinOsc AR f 0 * 0.1
>         t = impulse KR 4 0
>         pf = poll t f 0 (label "f")
>         px = poll t x 0 (label "x")
>     in mce [out 0 (mce2 o r),pf,px]
