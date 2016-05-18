    > Sound.SC3.UGen.Help.viewSC3Help "Pulse"
    > Sound.SC3.UGen.DB.ugenSummary "Pulse"

> import Sound.SC3 {- hsc3 -}

Modulate frequency

> g_01 =
>     let f = xLine KR 40 4000 6 RemoveSynth
>     in pulse AR f 0.1 * 0.1

Modulate pulse width

> g_02 =
>     let w = line KR 0.01 0.99 8 RemoveSynth
>     in pulse AR 200 w * 0.1

Two band limited square waves through a resonant low pass filter

> g_03 =
>     let p = pulse AR (mce2 100 250) 0.5 * 0.1
>         f = xLine KR 8000 400 5 DoNothing
>     in rlpf p f 0.05
