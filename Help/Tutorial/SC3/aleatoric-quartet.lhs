aleatoric quartet

> let { chain_of n p = foldl (flip (.)) id (replicate n p)
>     ; rapf i = do { i' <- i
>                   ; r <- clone 2 (rand 0 0.05)
>                   ; return (allpassN i' 0.05 r 1) }
>     ; t n = let { amp = 0.4
>                 ; density = mouseX KR 0.01 1 Linear 0.1
>                 ; dmul = recip density * 0.5 * amp
>                 ; dadd = amp - dmul }
>             in return . sum =<< replicateM n (s dmul dadd)
>     ; s dm da = do { r0 <- rand (-1) 1
>                    ; f' <- f
>                    ; x' <- x dm da
>                    ; return (pan2 (combL x' 0.02 (recip f') 3) r0 1) }
>     ; f = do { i0 <- iRand 0 2
>              ; let r0 = select i0 (mce [1, 0.5, 0.25])
>                in do { n0 <- lfNoise0 KR r0
>                      ; r1 <- rand (-30) 30
>                      ; let m = lag (roundE (n0 * 7 + (66 + r1)) 1) 0.2
>                        in return (midiCPS m) } }
>     ; x dm da = do { n0 <- pinkNoise AR
>                    ; n1 <- lfNoise1 KR 8
>                    ; return (n0 * max 0 (n1 * dm + da)) } }
> in do { g <- chain_of 5 rapf (t 4)
>       ; audition (out 0 (leakDC g 0.995)) }
