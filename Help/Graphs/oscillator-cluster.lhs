oscillator cluster (rd)

> let { rng i l r = linLin i (-1) 1 l r
>     ; ln a b d = line KR a b d RemoveSynth
>     ; xln a b d = xLine KR a b d RemoveSynth
>     ; rln r a b d = liftM (\n -> ln (a + n) b d) (rand 0 r)
>     ; rxln r a b d = liftM (\n -> xln (a + n) b d) (rand 0 r)
>     ; prt d a cf = do { r1 <- rand cf (cf + 2)
>                       ; r2 <- rln 1 5 0.01 d
>                       ; r3 <- rln 10 20 0 d
>                       ; r4 <- rand 0.1 0.2
>                       ; let { f = mce2 cf r1 + sinOsc KR r2 0 * r3
>                             ; o = fSinOsc AR f 0
>                             ; e = decay2 (impulse AR 0 0) r4 d * a }
>                        in return (o * e) }
>     ; np = 12
>     ; fp = replicateM np (rand 220 660) }
> in do { d <- rand 4 7
>       ; a <- rand 0.01 0.05
>       ; audition . (out 0) . sum =<< mapM (prt d a) =<< fp }
