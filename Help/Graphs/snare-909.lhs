snare-909 (jmcc)

> import Sound.SC3.Monadic

> main =
>   let { snr tr n v =
>         let { e a b = envGen ar tr 1 0 1 DoNothing (envPerc a b)
>             ; e1 = e 0.0005 0.055
>             ; e2 = e 0.0005 0.075
>             ; e3 = e 0.0005 0.4
>             ; e4 = e 0.0005 0.283
>             ; t1 = lfTri ar 330 0
>             ; t2 = lfTri ar 185 0
>             ; x1 = lpf n 7040 * 0.1 + v
>             ; x2 = hpf x1 523
>             ; m1 = t1 * e1 * 0.25 + t2 * e2 * 0.25
>             ; m2 = x1 * e3 * 0.20 + x2 * e4 * 0.20 }
>         in m1 + m2
>       ; x = mouseX kr 1 4 Linear 0.2
>       ; y = mouseY kr 0.25 0.75 Exponential 0.2
>       ; t = impulse kr (3 * x) 0 }
>   in do { n <- whiteNoise ar
>         ; v <- tRand 0.25 1.0 t
>         ; audition (out 0 (pan2 (snr t n v) 0 y)) }

{ var snr =
      { arg tr, n, v
      ; var e = { arg a, b
                ; EnvGen.ar(Env.perc(a, b), tr, 1, 0, 1, 0) }
      ; var e1 = e.value(0.0005, 0.055)
      ; var e2 = e.value(0.0005, 0.075)
      ; var e3 = e.value(0.0005, 0.4)
      ; var e4 = e.value(0.0005, 0.283)
      ; var t1 = LFTri.ar(330, 0)
      ; var t2 = LFTri.ar(185, 0)
      ; var x1 = LPF.ar(n, 7040) * (0.1 + v)
      ; var x2 = HPF.ar(x1, 523)
      ; var m1 = (t1 * e1 * 0.25) + (t2 * e2 * 0.25)
      ; var m2 = (x1 * e3 * 0.20) + (x2 * e4 * 0.20)
      ; m1 + m2 }
; var x = MouseX.kr(1, 4, 'linear', 0.2)
; var y = MouseY.kr(0.25, 0.75, 'exponential', 0.2)
; var t = Impulse.kr(3 * x, 0)
; var n = WhiteNoise.ar
; var v = TRand.kr(0.25, 1.0, t)
; Out.ar(0, Pan2.ar(snr.value(t, n, v), 0, y)) }.play
