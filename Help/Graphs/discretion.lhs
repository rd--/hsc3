discretion

> let { mkls bp t = envGen KR 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
>     ; rrand l r = getStdRandom (randomR (l, r))
>     ; part = do { [f1, f3] <- let f = liftM MCE (replicateM 2 (rrand 50 55))
>                               in replicateM 2 f
>                 ; f2 <- liftM MCE (replicateM 2 (rrand 50 65))
>                 ; a <- liftM MCE (replicateM 2 (rrand 0.01 0.035))
>                 ; let { t = 21
>                       ; f_ = mkls [(0, f1), (0.33, f2), (1, f3)] t
>                       ; a_ = mkls [(0, 0), (0.33, a), (1, 0)] t }
>                   in return (saw AR f_ * a_) }
>     ; parts = liftM sum (replicateM 8 part) }
> in audition . out 0 =<< parts
