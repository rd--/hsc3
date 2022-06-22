    Sound.SC3.UGen.Help.viewSC3Help "PV_MagMap"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagMap"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 = pinkNoiseId 'α' ar * 0.03 + sinOsc ar 440 0 * 0.5

> f_01 sig map_buf =
>     let c0 = fft' (localBufId 'β' 1 2048) sig
>         c1 = pv_MagMap c0 map_buf
>     in ifft' c1

> f_02 l t c = envelope_table 256 (Envelope l t [c] Nothing Nothing 0)

the curve to map the sound onto

> t_01 :: (Ord t,Floating t,Enum t) => [t]
> t_01 = f_02 [0,1,0] [0.05,0.95] EnvWelch

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_p1_ln [t_01]

> f_03 t l c = withSC3 (sendMessage (b_setn1 10 0 (f_02 t l c)))

    withSC3 (async (b_alloc 10 256 1) >> sendMessage (b_setn1 10 0 t_01))
    f_03 [0,1] [1] EnvLin
    f_03 [1,0] [1] EnvLin

> g_03 = f_01 g_01 10

localBuf fails...

> g_09 = f_01 g_01 (asLocalBufId 'γ' t_01)

