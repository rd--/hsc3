f-lets (rd)

> import Sound.SC3.Monadic

> main =
>   let { f_let t g j n f = 
>         let pd = pulseDivider t j 0
>         in do { r0 <- tiRand (mce2 2 1) n pd
>               ; r1 <- tRand 0.01 0.04 pd
>               ; r2 <- tRand 0.05 0.10 pd
>               ; return (formlet pd (f * r0) r1 r2 * g) }
>       ; mk_n t = do { r0 <- tRand 0 1 t
>                     ; r1 <- tRand 0 1 t
>                     ; r2 <- tRand 0 1 t
>                     ; r3 <- tRand 0 1 t
>                     ; r4 <- coinGate 0.2 t
>                     ; sequence
>                       [ f_let t 0.15 2 9 (mce2 200 400)
>                       , f_let t 0.25 2 9 (mce2 (200 + r0) (400 + r1))
>                       , f_let t 0.05 4 5 (mce2 25 50)
>                       , f_let t 0.15 4 5 (mce2 (25 + r2) (50 + r3))
>                       , let lr = fmap (* (latch r4 t))
>                         in lr (f_let t 0.5 1 16 (mce2 300 600)) ] }
>       ; tr = impulse ar 24 0 }
>   in do { n <- lfNoise0 kr 2
>         ; audition . out 0 . (* (n * 0.25 + 0.25)) . sum =<< mk_n tr }
