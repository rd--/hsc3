klink (rd)

> do { n1 <- lfNoise0 KR (mce2 0.5 1.5)
>    ; let { o = sinOsc KR n1 0
>          ; f = mce2 2 3
>          ; a = abs (slope o) * f
>          ; t = impulse AR a 0
>          ; i = decay2 t 0.01 0.1
>          ; x = mouseX KR 960 3620 Exponential 0.2
>          ; y = mouseY KR 0.5 2.0 Linear 0.2 }
>      in do { n2 <- tRand x 3940 t
>            ; n3 <- tRand 0.005 0.275 t
>            ; audition (out 0 (ringz i n2 (n3 * y))) } }

{ var n1 = LFNoise0.kr([0.5, 1.5])
; var o = SinOsc.kr(n1, 0)
; var f = [2, 3]
; var a = Slope.kr(o).abs * f
; var t = Impulse.ar(a, 0)
; var i = Decay2.ar(t, 0.01, 0.1)
; var x = MouseX.kr(960, 3620, 'exponential', 0.2)
; var y = MouseY.kr(0.5, 2.0, 'linear', 0.2)
; var n2 = TRand.ar(x, 3940, t)
; var n3 = TRand.ar(0.005, 0.275, t)
; Out.ar(0, Ringz.ar(i, n2, n3 * y)) }.play

(let* ((n1 (LFNoise0 kr (mce2 0.5 1.5)))
       (o (SinOsc kr n1 0))
       (f (mce2 2 3))
       (a (Mul (Abs (Slope o)) f))
       (t (Impulse ar a 0))
       (i (Decay2 t 0.01 0.1))
       (x (MouseX kr 960 3620 1 0.1))
       (y (MouseY kr 0.5 2.0 0 0.2))
       (n2 (TRand x 3940 t))
       (n3 (TRand 0.005 0.275 t)))
  (audition (Out 0 (Ringz i n2 (Mul n3 y)))))
