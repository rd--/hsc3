shifting pulses (rd)

> import Sound.SC3.Monadic

> main =
>   do { [n0, n1, n2] <- sequence (replicate 3 (clone 2 (brownNoise kr)))
>      ; t <- dust kr 0.75
>      ; let { warp i = linLin i (-1) 1
>            ; l = latch t t
>            ; p = pulse ar (warp n0 2 (mce2 11 15)) 0.01 * 0.1 
>            ; f = warp n1 300 1800 
>            ; rq = warp n2 0.01 2 }
>        in audition (out 0 (l * rlpf p f rq)) }

{ var n0 = BrownNoise.kr.dup
; var n1 = BrownNoise.kr.dup
; var n2 = BrownNoise.kr.dup
; var t = Dust.kr(0.75)
; var l = Latch.kr(t, t)
; var p = Pulse.ar(n0.range(2, [11, 15]), 0.01) * 0.1 
; var f = n1.range(300, 1800)
; var rq = n2.range(0.01, 2)
; Out.ar(0, l * RLPF.ar(p, f, rq)) }.play
