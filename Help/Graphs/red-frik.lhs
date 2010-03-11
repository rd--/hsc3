red frik (f0)

> import Sound.SC3.Monadic

> main =
>   let red tr n = 
>         do { r1 <- tRand 0.3 3 tr
>            ; r2 <- tRand 0.3 5 tr
>            ; r3 <- tRand 0 0.5 tr
>            ; r4 <- tRand 0.49 0.56 tr
>            ; r5 <- tRand 0.3 0.6 tr
>            ; r6 <- tRand 0.3 0.5 tr
>            ; let { o1 = fSinOsc kr r2 0 * r3 + r4
>                  ; o2 = fSinOsc kr o1 r5 * r6 }
>              in return (rhpf n r1 o2) }
>     in do { n <- clone 2 (brownNoise ar)
>           ; let tr = impulse kr 0.1 0
>             in audition . out 0 =<< red tr n }

{ var red = { arg tr, n
            ; var r1 = TRand.kr(0.3, 3, tr)
            ; var r2 = TRand.kr(0.3, 5, tr)
            ; var r3 = TRand.kr(0, 0.5, tr)
            ; var r4 = TRand.kr(0.49, 0.56, tr)
            ; var r5 = TRand.kr(0.3, 0.6, tr)
            ; var r6 = TRand.kr(0.3, 0.5, tr)
            ; var o1 = FSinOsc.kr(r2, 0, r3, r4)
            ; var o2 = FSinOsc.kr(o1, r5, r6)
            ; RHPF.ar(n, r1, o2) }
; var n = [BrownNoise.ar, BrownNoise.ar]
; var tr = Impulse.kr(0.1, 0)
; Out.ar(0, red.value(tr, n)) }.play
