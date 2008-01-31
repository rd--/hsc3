ccomb (rd)

> let { rng l r i = linLin i (-1) 1 l r
>     ; lwr = 48
>     ; flwr = midiCPS lwr
>     ; spart t = do { n <- liftM (rng lwr 72.0) (lfNoise2 KR 0.1)
>                    ; e <- liftM (decay2 t 0.01) (tRand 0.05 0.75 t)
>                    ; x <- liftM (* e) (whiteNoise AR)
>                    ; m <- lfNoise2 KR 0.1
>                    ; let f = lag (midiCPS n) 0.25
>                      in return (combC x (recip flwr) (recip f) (rng 1 8 m)) } }
> in do { t <- dust KR (mce2 0.75 0.35)
>       ; audition . (out 0) . (* 0.1) . sum =<< replicateM 12 (spart t) }
