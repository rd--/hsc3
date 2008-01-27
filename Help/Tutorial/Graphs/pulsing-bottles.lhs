pulsing bottles

> let { r = do { n <- whiteNoise AR
>              ; r0 <- rand 4 14
>              ; r1 <- rand 0 0.7
>              ; r2 <- rand 400 7400
>              ; return (resonz (n * lfPulse KR r0 0 0.25 * r1) r2 0.01) }
>     ; s = do { f <- rand 0.1 0.5
>              ; p <- rand 0 (pi * 2)
>              ; return (sinOsc KR f p) }
>     ; u = do { r' <- r
>              ; s' <- s
>              ; return (pan2 r' s' 1) } }
> in audition . out 0 . sum =<< replicateM 6 u
