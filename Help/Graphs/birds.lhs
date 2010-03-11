birds (jmcc)

> import Sound.SC3.Monadic

> main =
>   let { node = do { r1 <- rand 94.0 102.0
>                   ; r2 <- rand (-1.5) 1.5
>                   ; r3 <- rand 0.0 1.0
>                   ; r4 <- rand 11.0 15.0
>                   ; r5 <- rand 0.0 1.0
>                   ; r6 <- rand 12.0 15.6
>                   ; r7 <- rand 0.0 1.0
>                   ; r8 <- rand (-1.0) 1.0
>                   ; let { f = r1 + lag (lfSaw AR (7 + r2) r3 * r4) 0.1
>                         ; a = lfPulse KR (1.0 / r6) r7 0.16 * 0.05
>                         ; b = sinOsc AR (midiCPS f) r5 * a } 
>                     in return (rotate2 b (silent 1) r8) }
>       ; apf i = do { r1 <- rand 0.0 0.06
>                    ; r2 <- rand 0.7 2.0
>                    ; return (allpassL i 0.07 r1 r2) }
>       ; composeM [] i = return i
>       ; composeM (f:fs) i = f =<< composeM fs i
>       ; chainM n f = composeM (replicate n f) }
>   in do { d <- return . sum =<< sequence (replicate 6 node)
>         ; w <- chainM 12 apf d
>         ; audition (out 0 (d * 0.7 + w * 0.3)) }

http://lists.create.ucsb.edu/pipermail/sc-users/2007-April/033239.html
