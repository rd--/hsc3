Sound.SC3.UGen.Help.viewSC3Help "Compander"
Sound.SC3.UGen.DB.ugenSummary "Compander"

> import Sound.SC3

Example signal to process.

> z =
>     let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>         p = mix (pulse AR (mce [80, 81]) 0.3)
>     in e * p

> z' = soundIn 2

    audition (out 0 z)

Noise gate (no hold, no hysteresis)

> g_01 =
>     let x = mouseX KR 0.01 0.15 Linear 0.1
>     in mce [z, compander z z x 10 1 0.002 0.15]

Compressor

> g_02 =
>     let x = mouseX KR 0.01 1 Linear 0.1
>     in mce [z, compander z z x 1 (1/3) 0.01 0.01]

Expander

> g_03 =
>     let x = mouseX KR 0.01 1 Linear 0.1
>     in mce [z, compander z z x 1 3 0.01 0.1]

Limiter

> g_04 =
>     let x = mouseX KR 0.01 1 Linear 0.1
>     in mce [z, compander z z x 1 (1/10) 0.01 0.01]

Sustainer

> g_05 =
>     let x = mouseX KR 0.01 0.15 Linear 0.1
>     in mce [z, compander z z x (1/3) 1.0 0.01 0.05]
