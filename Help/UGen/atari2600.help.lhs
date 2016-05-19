    > Sound.SC3.UGen.Help.viewSC3Help "Atari2600"
    > Sound.SC3.UGen.DB.ugenSummary "Atari2600"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Lang.Pattern {- hsc3-lang -}

> gr_00 = atari2600 1 2 3 4 5 5 1

> gr_01 = atari2600 2 3 10 10 5 5 1

> gr_02 =
>     let x = mouseX KR 0 15 Linear 0.1
>         y = mouseY KR 0 15 Linear 0.1
>     in atari2600 x y 10 10 5 5 1

> gr_03 =
>     let x = mouseX KR 0 31 Linear 0.1
>         y = mouseY KR 0 31 Linear 0.1
>     in atari2600 2 3 x y 5 5 1

> gr_04 =
>     let x = mouseX KR 0 15 Linear 0.1
>         y = mouseY KR 0 15 Linear 0.1
>     in atari2600 2 3 10 10 x y 1

> gr_05 =
>     let x = mouseX KR 0 15 Linear 0.1
>         o1 = sinOsc KR 0.35 0 * 7.5 + 7.5
>         y = mouseY KR 0 31 Linear 0.1
>         o2 = sinOsc KR 0.3 0 * 5.5 + 5.5
>     in atari2600 x o1 10 y o2 5 1

> ati_syn =
>     let gate' = control KR "gate" 1
>         tone0 = control KR "tone0" 5
>         tone1 = control KR "tone1" 8
>         freq0 = control KR "freq0" 10
>         freq1 = control KR "freq1" 20
>         rate = control KR "rate" 1
>         amp = control KR "amp" 1
>         pan = control KR "pan" 0
>         e = envASR 0.01 amp 0.05 (EnvNum (-4))
>         eg = envGen KR gate' 1 0 1 RemoveSynth e
>         z = atari2600 tone0 tone1 freq0 freq1 15 15 rate
>         o = out 0 (pan2 (z * eg) pan 1)
>     in synthdef "atari2600" o

> p_01 =
>     [(K_instr,psynth ati_syn)
>     ,(K_dur,0.125)
>     ,(K_amp,0.5)
>     ,(K_param "tone0",pseq [pn 3 64,pn 2 128,pn 10 8] inf)
>     ,(K_param "tone1",pseqn [32,12] [8,pwhite 'α' 0 15 inf] inf)
>     ,(K_param "freq0",pseqn [17,4,3] [10,prand 'β' [1,2,3] inf,10] inf)
>     ,(K_param "freq1",pseq1 [10,3,pwrand 'γ' [20,1] [0.6,0.4] inf] inf)]

    paudition (pbind p_01)

> p_02 =
>     [(K_instr,psynth ati_syn)
>     ,(K_dur,pseq [0.25,0.25,0.25,0.45] inf)
>     ,(K_amp,0.5)
>     ,(K_param "tone0",pseq [pseq [2,5] 32,pseq [3,5] 32] inf)
>     ,(K_param "tone1",14)
>     ,(K_param "freq0",pseq [pbrown 'α' 28 31 1 32,pbrown 'β' 23 26 3 32] inf)
>     ,(K_param "freq1",pseq [pn 10 16,pn 11 16] inf)]

    paudition (pbind p_03)

> p_03 =
>     [(K_instr,psynth ati_syn)
>     ,(K_dur,pbrown 'α' 0.1 0.15 0.1 inf)
>     ,(K_amp,0.5)
>     ,(K_param "tone0",1)
>     ,(K_param "tone1",2)
>     ,(K_param "freq0",pseqn [2,1] [24,pwrand 'β' [20,23] [0.6,0.4] inf] inf)
>     ,(K_param "freq1",pseqn [1,1,1] [1,3,pwrand 'γ' [2,1] [0.6,0.4] inf] inf)]

    paudition (pbind p_03)
