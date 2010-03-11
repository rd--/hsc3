s-chirp (rd)

> import Sound.SC3.Monadic

> main =
>   let { x = mouseX kr 15 0 Linear 0.1
>       ; y = mouseY kr 15 27 Linear 0.1
>       ; scl = [0, 2, 3.2, 5, 7, 9, 10] }
>   in do { t <- dust kr 9
>         ; b <- tChoose t (mce [36, 48, 60, 72])
>         ; n <- fmap (* 0.04) (lfNoise1 kr (mce2 3 3.05))
>         ; d <- tiRand x y t
>         ; e <- fmap (decay2 t 0.005) (tRand 0.02 0.15 t)
>         ; o <- let { k = degreeToKey 0 d 12
>                    ; f = midiCPS (b + k + n)
>                    ; m = e * sinOsc ar f 0 * 0.2
>                    ; u = pulseDivider t 9 0 }
>                in do { r0 <- tRand 0.0075 0.125 u
>                      ; r1 <- tRand 0.05 0.15 u
>                      ; return (m * 0.5 + allpassC m 0.15 r0 r1) }
>         ; withSC3 (\fd -> do { async fd (b_alloc 0 7 1)
>                              ; send fd (b_setn1 0 0 scl)
>                              ; play fd (out 0 o) }) }
