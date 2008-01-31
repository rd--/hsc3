cricket (rd)

> let { mtrand l r t = liftM mce (replicateM 2 (tRand l r t))
>     ; rrand l r = getStdRandom (randomR (l, r))
>     ; mrand l r = liftM mce (replicateM 2 (rrand l r)) }
> in do { r1 <- mrand 10 13
>       ; r2 <- mrand 10 13
>       ; r3 <- mrand 4 7
>       ; let { t = impulse KR 0.7 0
>             ; e = decay2 (impulse KR r2 0) 0.001 0.005
>             ; f = sinOsc KR r1 0 * e * r3 }
>         in do { r4 <- mtrand 2220 2227 t
>               ; audition (out 0 (sinOsc AR r4 0 * f * 0.25)) } }
