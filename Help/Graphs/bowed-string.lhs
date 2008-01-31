bowed string (jmcc)

> do { n0 <- brownNoise AR
>    ; r0 <- expRand 0.125 0.5
>    ; n1 <- lfNoise1 KR r0
>    ; let x = n0 * mce2 0.007 0.0071 * max 0 (n1 * 0.6 + 0.4)
>      in do { r1 <- rand 0.7 0.9
>            ; r2 <- replicateM 12 (rand 1.0 3.0)
>            ; r3 <- rand 20 360
>            ; let { geom n i f = take n (iterate (* f) i)
>                  ; iota n i s = take n (iterate (+ s) i)
>                  ; d = klankSpec (iota 12 r3 r3) (geom 12 1 r1) r2
>                  ; k = klank x 1 0 1 d }
>              in audition (out 0 (softClip (k * 0.1))) } }
