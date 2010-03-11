synthetic piano (jmcc)

> import Sound.SC3.Monadic

> main =
>   let p = do { n <- iRand 36 90
>              ; f <- rand 0.1 0.5
>              ; ph <- rand 0 (pi * 2)
>              ; let { s = impulse ar f ph * 0.1
>                    ; e = decay2 s 0.008 0.04
>                    ; c i = do { n0 <- lfNoise2 ar 3000
>                               ; let { o = [-0.05, 0, 0.04] !! i
>                                     ; dt = 1 / (midiCPS (n + o)) }
>                                 in return (combL (n0 * e) dt dt 6) }
>                    ; l = ((n - 36) / 27) - 1 }
>                in do { c_ <- mixFillM 3 c
>                      ; return (pan2 c_ l 1) } }
>   in audition . out 0 =<< mixFillM 6 (const p)

{ var p = { var n = IRand.new(36, 90)
          ; var f = Rand.new(0.1, 0.5)
          ; var ph = Rand.new(0, pi * 2)
          ; var s = Impulse.ar(f, ph) * 0.1
          ; var e = Decay2.ar(s, 0.008, 0.04)
          ; var c = { arg i
                    ; var n0 = LFNoise2.ar(3000)
                    ; var o = [-0.05, 0, 0.04].at(i)
                    ; var dt = 1 / (n + o).midicps
                    ; CombL.ar(n0 * e, dt, dt, 6) }
          ; var l = ((n - 36) / 27) - 1
          ; var c_ = Mix.fill(3, c)
          ; Pan2.ar(c_, l, 1) }
; Out.ar(0, Mix.fill(6, p)) }.play
