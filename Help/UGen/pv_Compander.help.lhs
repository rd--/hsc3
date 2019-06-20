> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

Example signal to process.

> g_01 =
>     let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>         p = mix (pulse AR (mce [80, 81]) 0.3)
>     in e * p

> g_02 = soundIn 0

mostly compress

> f_01 z =
>     let x = mouseX KR 1 50 Linear 0.2
>     in mce [z, ifft' (pv_Compander (fft' (localBuf 'α' 2048 1) z) x 1.2 0.25)]

> g_03 = f_01 g_01

moslt expand

> f_02 z =
>     let x = mouseX KR 1 50 Linear 0.1
>     in mce [z, ifft' (pv_Compander (fft' (localBuf 'β' 2048 1) z) x 2.0 0.85)]

> g_04 = f_02 g_01

pv sustainer

> f_03 z =
>   let x = mouseX KR 1 80 Linear 0.1
>       s = ifft' (pv_Compander (fft' (localBuf 'γ' 2048 1) z) x 0.5 1.0)
>   in mce [z, limiter s 0.999 0.05]

> g_05 = f_03 g_01
