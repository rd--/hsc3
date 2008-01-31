mouse clatter (rd)

> let { x = mouseX KR 100 12000 Linear 0.1
>     ; y = mouseY KR 0.01 0.15 Linear 0.1 }
> in do { n1 <- lfNoise0 KR (mce [3, 3.25])
>       ; let { t = impulse KR (n1 * 16 + 18) 0
>             ; s = do { n2 <- tRand 0.005 y t
>                      ; n3 <- whiteNoise AR
>                      ; n4 <- tRand 10 x t
>                      ; n5 <- tRand 0 1 t
>                      ; n6 <- tExpRand 0.15 1 t
>                      ; o <- let e = decay2 t 0.01 n2
>                             in return (bpf (n3 * e) n4 n5)
>                      ; n7 <- pv_RandComb (fft' 10 o) n6 t
>                      ; return (o * 0.05 + ifft' n7) } }
>         in withSC3 (\fd -> do { async fd (b_alloc 10 2048 1)
>                               ; play fd . out 0 =<< s }) }
