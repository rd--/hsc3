what was i thinking? (jmcc)

> let { z = do { n0 <- lfNoise1 KR 0.2
>              ; n1 <- lfNoise1 KR 0.157
>              ; let { p = pulse AR f (n1 * 0.4 + 0.5) * 0.04
>                    ; i = lfPulse AR 0.1 0 0.05 * impulse AR 8 0 * 500
>                    ; d = decay i 2
>                    ; f = max (sinOsc KR 4 0 + 80) d }
>                in return (rlpf p (n0 * 2000 + 2400) 0.2) }
>     ; c i = do { r <- rand 0 0.3
>                ; n <- lfNoise1 KR r
>                ; return (combL i 0.06 (n * 0.025 + 0.035) 1) } }
> in do { z0 <- z
>       ; z1 <- liftM (* 0.6) z
>       ; f0 <- clone 2 (c z1)
>       ; f1 <- clone 2 (c z1)
>       ; audition (out 0 (z0 + mce [mix f0, mix f1])) }
