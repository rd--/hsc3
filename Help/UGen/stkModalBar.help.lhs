    Sound.SC3.UGen.Help.viewSC3Help "StkModalBar"
    Sound.SC3.UGen.DB.ugenSummary "StkModalBar"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkModalBar {- hsc3-rec -}

instrument: Marimba = 0, Vibraphone = 1, Agogo = 2, Wood1 = 3, Reso = 4, Wood2 = 5, Beats = 6, Two Fixed = 7, Clump = 8

> g_01 =
>   let x = mouseX KR 0.25 12 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       u = stkModalBarR
>         {freq = midiCPS (tiRand 'α' 25 96 tr)
>         ,instrument = tiRand 'β' 0 9 tr
>         ,stickhardness = tRand 'γ' 0 127 tr
>         ,stickposition = tRand 'δ' 0 127 tr
>         ,vibratogain = tRand 'ε' 0 127 tr
>         ,vibratofreq = tRand 'ζ' 0 127 tr
>         ,directstickmix = tRand 'η' 0 127 tr
>         ,volume = tRand 'θ' 0 127 tr
>         ,trig_ = tr}
>   in mkStkModalBar u

> g_02 =
>   let x = mouseX KR 1 12 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       tr3 = pulseDivider tr 3 0
>       u = stkModalBarR
>         {freq = midiCPS (tiRand 'α' 52 64 tr)
>         ,instrument = 1
>         ,stickhardness = tRand 'β' 10 50 tr3
>         ,stickposition = tRand 'γ' 40 80 tr3
>         ,vibratogain = tRand 'δ' 66 98 tr3
>         ,vibratofreq = tRand 'ε' 4 12 tr3
>         ,directstickmix = tRand 'ζ' 0 1 tr3
>         ,volume = tRand 'η' 16 48 tr3
>         ,trig_ = tr}
>   in mkStkModalBar u

<https://ccrma.stanford.edu/software/stk/classstk_1_1ModalBar.html>
