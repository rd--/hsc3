reverberated sine percussion

> let { s d = do { n <- dust AR (2 / d)
>                ; r <- rand 0 3000
>                ; return (resonz (n * 50) (200 + r) 0.003) }
>     ; ss d = return . sum =<< replicateM d (s (constant d))
>     ; z i = delayN i 0.048 0.48
>     ; y c i = do { r <- clone c (rand 0 0.1)
>                  ; n <- lfNoise1 KR r
>                  ; return (mix (combL i 0.1 (n * 0.04 + 0.05) 15)) }       
>     ; x i = do { y' <- i
>                ; r <- clone 2 (rand 0 0.05)
>                ; return (allpassN y' 0.05 r 1) } }
> in do { s' <- ss 10
>       ; x' <- foldl (flip (.)) id [x,x,x,x] (y 7 (z s'))
>       ; audition (out 0 (s' + x' * 0.2)) }
