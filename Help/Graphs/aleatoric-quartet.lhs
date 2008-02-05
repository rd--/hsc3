aleatoric quartet (jmcc)

> let { amp = 0.07
>     ; density = mouseX KR 0.01 1 Linear 0.1
>     ; dmul = recip density * 0.5 * amp
>     ; dadd = amp - dmul
>     ; chain n f = foldl (>=>) return (replicate n f)
>     ; rapf i = do { r <- clone 2 (rand 0 0.05)
>                   ; return (allpassN i 0.05 r 1) }
>     ; mk_f = do { i0 <- iRand 0 2
>                 ; let r0 = select i0 (mce [1, 0.5, 0.25])
>                   in do { r1 <- rand (-30) 30
>                         ; n0 <- lfNoise0 KR r0
>                         ; let m = lag (roundE (n0 * 7 + 66 + r1) 1) 0.2
>                           in return (midiCPS m) } }
>     ; mk_s = do { f <- fmap recip mk_f
>                 ; r <- rand (-1) 1
>                 ; x <- do { n0 <- pinkNoise AR
>                           ; n1 <- lfNoise1 KR 8
>                           ; return (n0 * max 0 (n1 * dmul + dadd)) }
>                 ; return (pan2 (combL x 0.02 f 3) r 1) } }
> in do { g <- chain 5 rapf =<< fmap sum (replicateM 4 mk_s)
>       ; audition (out 0 (leakDC g 0.995)) }
