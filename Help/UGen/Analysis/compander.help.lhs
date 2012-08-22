> Sound.SC3.UGen.Help.viewSC3Help "Compander"
> Sound.SC3.UGen.DB.ugenSummary "Compander"

> import Sound.SC3

Example signal to process.
> let z = let {e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>             ;p = mix (pulse AR (mce [80, 81]) 0.3)}
>         in e * p

> audition (out 0 z)

Noise gate
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 10 1 0.01 0.01]))

Compressor
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 1 0.5 0.01 0.01]))

Limiter
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 1 0.1 0.01 0.01]))

Sustainer
> let x = mouseX KR 0.01 1 Linear 0.1
> in audition (out 0 (mce [z, compander z z x 0.1 1.0 0.01 0.01]))
