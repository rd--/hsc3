eggcrate (rd)

> import Sound.SC3.Monadic

> main =
>   let { cosu = cos . (* pi) 
>       ; sinu = sin . (* pi)
>       ; eggcrate u v = cosu u * sinu v
>       ; p = mce [64, 72, 96, 128, 256, 6400, 7200, 8400, 9600] }
>   in do { [x, y] <- sequence (replicate 2 (brownNoise kr))
>         ; t <- dust kr 2.4
>         ; [f0, f1] <- sequence (replicate 2 (tChoose t p))
>         ; let { f = linLin (eggcrate x y) (-1) 1 f0 f1
>               ; a = linLin x (-1) 1 0 0.1 }
>           in audition (out 0 (pan2 (mix (sinOsc ar f 0)) y a)) }

{ var eggcrate = { arg u, v
                 ; (u * pi).cos * (v * pi).sin }
; var p = [64, 72, 96, 128, 256, 6400, 7200, 8400, 9600]
; var x = BrownNoise.kr()
; var y = BrownNoise.kr()
; var t = Dust.kr(2.4)
; var f0 = TChoose.kr(t, p)
; var f1 = TChoose.kr(t, p)
; var f = LinLin.kr(eggcrate.value(x, y), -1, 1, f0, f1)
; var a = LinLin.kr(x, -1, 1, 0, 0.1)
; Out.ar(0, Pan2.ar(Mix.ar(SinOsc.ar(f, 0)), y, a)) }.play
