    > Sound.SC3.UGen.Help.viewSC3Help "Osc"
    > Sound.SC3.UGen.DB.ugenSummary "Osc"

> import Sound.SC3 {- hsc3 -}

Allocate and generate wavetable buffer

    > withSC3 (do {_ <- async (b_alloc 10 512 1)
    >             ;let f = [Normalise,Wavetable,Clear]
    >              in send (b_gen_sine1 10 f [1,1/2,1/3,1/4,1/5])})

Fixed frequency wavetable oscillator

> g_01 = osc AR 10 220 0 * 0.1

Modulate frequency

> g_02 =
>     let f = xLine KR 2000 200 1 DoNothing
>     in osc AR 10 f 0 * 0.1

As frequency modulator

> g_03 =
>     let f = osc AR 10 (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
>     in osc AR 10 f 0 * 0.1

As phase modulatulator

> g_04 =
>     let p = osc AR 10 (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
>     in osc AR 10 800 p * 0.1

Fixed frequency wavetable oscillator

> g_05 = osc AR 10 220 0 * 0.1

Change the wavetable while its playing

    > let f = [Normalise,Wavetable,Clear]
    > in withSC3 (send (b_gen_sine1 10 f [1,0.6,1/4]))

Send directly calculated wavetable

    > import Sound.SC3.Common.Buffer {- hsc3 -}
    > import Sound.SC3.Common.Math.Window {- hsc3 -}
    > let t = to_wavetable (triangular_table 512)
    > withSC3 (send (b_setn1 10 0 t))
