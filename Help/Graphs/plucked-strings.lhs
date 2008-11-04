plucked strings (jmcc)

> import Sound.SC3
> import qualified Sound.SC3.UGen.Monadic as M

> let { s = do { n0 <- M.pinkNoise ar
>              ; r1 <- M.rand (-1) 1
>              ; im <- i
>              ; dt' <- dt
>              ; let t = decay im 0.1 * n0 * 0.1
>                in return (pan2 (combL t dt' dt' 4) r1 1) }
>     ; i = do { r0 <- M.rand 2 2.2
>              ; n0 <- M.dust ar 0.5
>              ; r1 <- M.rand 0.05 0.15
>              ; r2 <- M.rand 0 (pi * 2)
>              ; r3 <- M.iRand 0 2
>              ; let { s0 = impulse ar r0 0.3
>                    ; s1 = n0 * 0.3
>                    ; s2 = impulse ar (sinOsc kr r1 r2 * 5 + 5.2) 0.3 }
>                in return (select r3 (mce [s0, s1, s2])) }
>     ; dt = do { r0 <- M.rand 60 90
>               ; return (1 / (midiCPS (floorE r0))) } }
> in audition . out 0 . sum =<< sequence (replicate 5 s)
