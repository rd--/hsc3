aleatoric quartet

> let { chain n f = foldl (>=>) return (replicate n f)
>     ; rapf i = do { r <- clone 2 (rand 0 0.05)
>                   ; return (allpassN i 0.05 r 1) }
>     ; mk_f = do { i0 <- iRand 0 2
>                 ; let r0 = select i0 (mce [1, 0.5, 0.25])
>                   in do { n0 <- lfNoise0 KR r0
>                         ; r1 <- rand (-30) 30
>                         ; let m = lag (roundE (n0 * 7 + (66 + r1)) 1) 0.2
>                           in return (midiCPS m) } }
>     ; mk_s dm da = do { f <- mk_f
>                       ; r <- rand (-1) 1
>                       ; x <- do { n0 <- pinkNoise AR
>                                 ; n1 <- lfNoise1 KR 8
>                                 ; return (n0 * max 0 (n1 * dm + da)) }
>                       ; return (pan2 (combL x 0.02 (recip f) 3) r 1) }
>     ; t n = let { amp = 0.4
>                 ; density = mouseX KR 0.01 1 Linear 0.1
>                 ; dmul = recip density * 0.5 * amp
>                 ; dadd = amp - dmul }
>             in return . sum =<< replicateM n (mk_s dmul dadd) }
> in do { g <- chain 5 rapf =<< t 4
>       ; audition (out 0 (leakDC g 0.995)) }
