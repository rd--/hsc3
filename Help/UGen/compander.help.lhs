> import Sound.SC3 {- hsc3 -}

Example signal to process.

> g_01 =
>     let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>         p = mix (pulse AR (mce [80, 81]) 0.3)
>     in e * p

> g_02 = soundIn 0

Noise gate (no hold, no hysteresis)

> f_01 z =
>     let x = mouseX KR 0.01 0.15 Linear 0.1
>     in mce [z, compander z z x 10 1 0.002 0.15]

Compressor

> f_02 z =
>     let x = mouseX KR 0.01 1 Linear 0.1
>     in mce [z, compander z z x 1 (1/3) 0.01 0.01]

Expander

> f_03 z =
>     let x = mouseX KR 0.01 1 Linear 0.1
>     in mce [z, compander z z x 1 3 0.01 0.1]

Limiter

> f_04 z =
>     let x = mouseX KR 0.01 1 Linear 0.1
>     in mce [z, compander z z x 1 (1/10) 0.01 0.01]

Sustainer

> f_05 z =
>     let x = mouseX KR 0.01 0.15 Linear 0.1
>     in mce [z, compander z z x (1/3) 1.0 0.01 0.05]

> g_03 = f_01 g_01
> g_04 = f_02 g_01
> g_05 = f_03 g_01
> g_06 = f_04 g_01
> g_07 = f_05 g_01
