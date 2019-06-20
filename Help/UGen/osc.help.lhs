> import Sound.SC3 {- hsc3 -}

Allocate and generate wavetable buffer

> m_01 =
>   [b_alloc 10 512 1
>   ,b_gen_sine1 10 [Normalise,Wavetable,Clear] [1,1/2,1/3,1/4,1/5]]

    > withSC3 (mapM_ maybe_async m_01)

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

As phase modulator

> g_04 =
>     let p = osc AR 10 (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
>     in osc AR 10 800 p * 0.1

Fixed frequency wavetable oscillator

> g_05 = osc AR 10 220 0 * 0.1

Change the wavetable while its playing

    > withSC3 (maybe_async (b_gen_sine1 10 [Normalise,Wavetable,Clear] [1,0.6,1/4]))

Send directly calculated wavetable

    > import Sound.SC3.Common.Buffer {- hsc3 -}
    > import Sound.SC3.Common.Math.Window {- hsc3 -}
    > let t = to_wavetable (triangular_table 512)
    > withSC3 (maybe_async (b_setn1 10 0 t))
