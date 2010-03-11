three-cpsw (rd)

> import Sound.SC3.Monadic

> main =
>   do { t <- dust kr (mce2 12 18)
>      ; f0 <- tRand 1 64 t
>      ; f1 <- lfNoise0 kr f0
>      ; a <- tRand 0.0 0.5 t
>      ; dt <- tRand 0.975 1.025 t
>      ; dh <- tRand 0.750 0.7505 t
>      ; let { f = f1 * mce2 9000 12000 + 9500
>            ; o = saw ar f + saw ar (f * dh) + saw ar (f * dt) }
>        in audition (out 0 (clip2 (o * a) 0.75)) }

{ var t = Dust.kr([12, 18])
; var f0 = TRand.kr(1, 64, t)
; var f1 = LFNoise0.kr(f0)
; var a = TRand.kr(0.0, 0.5, t)
; var dt = TRand.kr(0.975, 1.025, t)
; var dh = TRand.kr(0.750, 0.7505, t)
; var f = f1 * [9000, 12000] + 9500
; var o = Saw.ar(f) + Saw.ar(f * dh) + Saw.ar(f * dt)
; Out.ar(0, (o * a).clip2(0.75)) }.play
