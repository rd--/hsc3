    > Sound.SC3.UGen.Help.viewSC3Help "LinExp"
    > Sound.SC3.UGen.DB.ugenSummary "LinExp"

> import Sound.SC3 {- hsc3 -}

{var mod = SinOsc.kr(Line.kr(1, 10, 10)); SinOsc.ar(mod * 400 + 500) * 0.1}.play

> g_01 =
>   let mod = sinOsc KR (line KR 1 10 10 DoNothing) 0
>   in sinOsc AR (mod * 400 + 500) 0 * 0.1

{var mod = SinOsc.kr(Line.kr(1, 10, 10)); SinOsc.ar(LinExp.kr(mod, -1,1, 100, 900)) * 0.1}.play

> g_02 =
>   let mod = sinOsc KR (line KR 1 10 10 DoNothing) 0
>   in sinOsc AR (linExp mod (-1) 1 100 900) 0 * 0.1

> g_03 =
>     let f = linExp (mouseX KR 0 1 Linear 0.2) 0 1 440 660
>     in sinOsc AR f 0 * 0.1

The destination range may be k-rate.

> g_04 =
>     let x = mouseX KR 0 1 Linear 0.2
>         y = mouseY KR 220 440 Linear 0.2
>         f = linExp x 0 1 y 660
>     in sinOsc AR f 0 * 0.1

i-rate...

> g_05 = sinOsc AR (linExp (rand 'α' 0 1) 0 1 220 440) 0 * 0.1
