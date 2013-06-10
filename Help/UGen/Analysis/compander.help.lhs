> Sound.SC3.UGen.Help.viewSC3Help "Compander"
> Sound.SC3.UGen.DB.ugenSummary "Compander"

> import Sound.SC3

Example signal to process.
> let z = let {e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>             ;p = mix (pulse AR (mce [80, 81]) 0.3)}
>         in e * p

> let z = soundIn 4

> audition (out 0 z)

Noise gate (no hold, no hysteresis)
> let x = mouseX KR 0.01 0.15 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 10 1 0.002 0.15]))

Compressor
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 1 (1/3) 0.01 0.01]))

Expander
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 1 3 0.01 0.1]))

Limiter
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 1 (1/10) 0.01 0.01]))

Sustainer
> let x = mouseX KR 0.01 0.15 Linear 0.1
> in audition (out 0 (mce [z, compander z z x (1/3) 1.0 0.01 0.05]))
