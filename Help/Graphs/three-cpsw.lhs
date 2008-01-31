three-cpsw

> do { t <- dust KR (mce2 12 18)
>    ; f0 <- tRand 1 64 t
>    ; f1 <- lfNoise0 KR f0
>    ; a <- tRand 0.0 0.5 t
>    ; dt <- tRand 0.975 1.025 t
>    ; dh <- tRand 0.750 0.7505 t
>    ; let { f = f1 * mce2 9000 12000 + 9500
>          ; o = saw AR f + saw AR (f * dh) + saw AR (f * dt) }
>      in audition (out 0 (clip2 (o * a) 0.75)) }
