> Sound.SC3.UGen.Help.viewSC3Help "FSinOsc"
> Sound.SC3.UGen.DB.ugenSummary "FSinOsc"

# SC2
The initial phase argument was not in the SC2 variant.

> import Sound.SC3

> audition (out 0 (fSinOsc AR (mce2 440 550) 0 * 0.05))

Modulate frequency
> audition (out 0 (fSinOsc AR (xLine KR 200 4000 1 RemoveSynth) 0 * 0.1))

Loses amplitude towards the end
> let f = fSinOsc AR (xLine KR 4 401 8 RemoveSynth)
> in audition (out 0 (fSinOsc AR (f 0 * 200 + 800) 0 * 0.1))

sin grain with sin envelope
> let {b = control IR "out" 0
>     ;f = control IR "freq" 440
>     ;d = control IR "dur" 0.2
>     ;a = control IR "amp" 0.1
>     ;p = control IR "pan" 0
>     ;o = fSinOsc AR f 0
>     ;s = envSine d a
>     ;e = envGen AR 1 1 0 1 RemoveSynth s
>     ;u = offsetOut b (pan2 o p e)
>     ;i = synthdef "grain" u}
> in withSC3 (async (d_recv i))

> import Sound.SC3.Lang.Pattern.ID

granular synthesis
> let p = pbind [("midinote",fmap fround (pbrown 'a' 72 84 1 inf))
>               ,("detune",pwhite 'a' 0 10 inf)
>               ,("dur",pbrown 'b' 0.005 0.15 0.05 inf)
>               ,("legato",pbrown 'c' 1 2 0.1 inf)
>               ,("amp",pbrown 'd' 0.05 0.25 0.05 inf)
>               ,("pan",pbrown 'e' (-1) 1 0.2 inf)]
> in audition ("grain",p)
