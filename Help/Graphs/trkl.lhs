trkl (rd)

> import Sound.SC3.Monadic

> main =
>   let trkl d ul fu dy la fy =
>           let { tf = xLine kr 1 ul d RemoveSynth
>               ; st = impulse ar (tf * 8) 0
>               ; t = impulse ar tf 0 }
>           in do { r0 <- rand (-1) 1
>                 ; r1 <- rand (-1) 1
>                 ; r2 <- tRand 0.05 1.0 t
>                 ; r3 <- tExpRand 0.0 0.25 st 
>                 ; fh <- tRand 1.75 2.25 t 
>                 ; let { a = dbAmp (line kr 12 la d RemoveSynth)
>                       ; f = xLine kr fu 900 d RemoveSynth
>                       ; p = line kr r0 r1 d RemoveSynth
>                       ; o1 = mix (ringz (decay2 t 0.01 dy) (mce2 f (f * fh)) fy)
>                       ; o2 = mix (saw ar (mce2 f (f * fh))) }
>                  in return (pan2 (o1 + o2 * decay2 t 0.1 r2 * r3) p a) }
>   in do { d <- rand 0.5 16
>         ; ul <- rand 16 64
>         ; fu <- rand 1200 9000
>         ; dy <- rand 0.005 0.175
>         ; la <- rand (-60) (-25)
>         ; fy <- rand 0.015 0.125
>         ; audition . (out 0) =<< trkl d ul fu dy la fy }
