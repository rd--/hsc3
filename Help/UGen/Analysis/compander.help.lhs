compander input control thresh slopeBelow slopeAbove clampTime relaxTime

Compressor, expander, limiter, gate, ducker.  General purpose dynamics
processor.

Example signal to process.

> let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     p = mix (pulse AR (MCE [80, 81]) 0.3)
> audition $ e * p

Noise gate

> let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     p = mix (pulse AR (MCE [80, 81]) 0.3)
>     z = e * p
>     x = mouseX KR 0.01 1 Linear 0.1
> audition $ MCE [z, compander z z x 1 0.01 0.01 0.1]

Compressor

> let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     p = mix (pulse AR (MCE [80, 81]) 0.3)
>     z = e * p
>     x = mouseX KR 0.01 1 Linear 0.1
> audition $ MCE [z, compander z z x 0.5 0.01 0.01 0.1]

Limiter

> let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     p = mix (pulse AR (MCE [80, 81]) 0.3)
>     z = e * p
>     x = mouseX KR 0.01 1 Linear 0.1
> audition $ MCE [z, compander z z x 0.1 0.01 0.01 0.1]

Sustainer

> let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
>     p = mix (pulse AR (MCE [80, 81]) 0.3)
>     z = e * p
>     x = mouseX KR 0.01 1 Linear 0.1
> audition $ MCE [z, compander z z x 1 0.01 0.01 0.1]
