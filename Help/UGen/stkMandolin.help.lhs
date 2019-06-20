> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkMandolin {- hsc3-rec -}

> g_01 =
>   let x = mouseX KR 0.25 16 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       u = stkMandolinR
>         {freq = midiCPS (tiRand 'α' 32 96 tr)
>         ,bodysize = tRand 'β' 0 127 tr
>         ,pickposition = tRand 'γ' 0 127 tr
>         ,stringdamping = tRand 'δ' 0 127 tr
>         ,stringdetune = tRand 'ε' 0 127 tr
>         ,aftertouch = tRand 'ζ' 0 127 tr
>         ,trig_ = tr}
>   in mkStkMandolin u

> g_02 =
>   let x = mouseX KR 3 16 Linear 0.2
>       tr = impulse KR x 0 - 0.5 -- trig
>       tr3 = pulseDivider tr 3 0
>       u = stkMandolinR
>         {freq = midiCPS (tiRand 'α' 54 66 tr)
>         ,bodysize = tRand 'β' 72 94 tr3
>         ,pickposition = tRand 'γ' 32 42 tr3
>         ,stringdamping = tRand 'δ' 64 72 tr3
>         ,stringdetune = tRand 'ε' 0 4 tr3
>         ,aftertouch = tRand 'ζ' 2 8 tr3
>         ,trig_ = tr}
>   in mkStkMandolin u
