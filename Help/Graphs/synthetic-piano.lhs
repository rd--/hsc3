synthetic piano

> let { s = do { f <- rand 0.1 0.5
>              ; p <- rand 0 (pi * 2)
>              ; return (impulse AR f p * 0.1) }
>     ; c e n o = do { n0 <- lfNoise2 AR 3000
>                    ; let dt = 1 / (midiCPS (n + o))
>                      in return (combL (n0 * e) dt dt 6) }
>     ; l n = ((n - 36) / 27) - 1
>     ; p = do { s' <- s
>              ; n <- iRand 36 90
>              ; let e = decay2 s' 0.008 0.04
>                in do { c' <- c e n (mce [-0.05, 0, 0.04])
>                      ; return (pan2 (mix c') (l n) 1) } } }
> in audition . out 0 . mix =<< clone 2 p
