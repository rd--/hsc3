> Sound.SC3.UGen.Help.viewSC3Help "Compander"
> Sound.SC3.UGen.DB.ugenSummary "Compander"

> import Sound.SC3

Example signal to process.
> let { e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     ; p = mix (pulse AR (mce [80, 81]) 0.3) }
> in audition (out 0 (e * p))

Noise gate
> let { e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     ; p = mix (pulse AR (mce [80, 81]) 0.3)
>     ; z = e * p
>     ; x = mouseX' KR 0.01 1 Linear 0.1 }
> in audition (out 0 (mce [z, compander z z x 10 1 0.01 0.01]))

Compressor
> let { e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     ; p = mix (pulse AR (mce [80, 81]) 0.3)
>     ; z = e * p
>     ; x = mouseX' KR 0.01 1 Linear 0.1 }
> in audition (out 0 (mce [z, compander z z x 1 0.5 0.01 0.01]))

Limiter
> let { e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     ; p = mix (pulse AR (mce [80, 81]) 0.3)
>     ; z = e * p
>     ; x = mouseX' KR 0.01 1 Linear 0.1 }
> in audition (out 0 (mce [z, compander z z x 1 0.1 0.01 0.01]))

Sustainer
> let { e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     ; p = mix (pulse AR (mce [80, 81]) 0.3)
>     ; z = e * p
>     ; x = mouseX' KR 0.01 1 Linear 0.1 }
> in audition (out 0 (mce [z, compander z z x 0.1 1.0 0.01 0.01]))
