lf pulses (rd)

> do { n0 <- lfNoise0 AR (mce2 20 40)
>    ; n1 <- lfNoise0 AR (mce2 5 10)
>    ; let { x = mouseX KR 0.012 0.19 Exponential 0.1
>          ; f = formlet (blip AR 10 12) (n0 * 43 + 700) 0.005 x 
>          ; o = sinOsc AR 40 0 * n1 }
>      in audition (out 0 (clip2 (leakDC (f + o) 0.995) 0.75)) }

{ var n0 = LFNoise0.ar([20, 40])
; var n1 = LFNoise0.ar([5, 10])
; var x = MouseX.kr(0.012, 0.19, 'exponential', 0.1)
; var f = Formlet.ar(Blip.ar(10, 12), n0 * 43 + 700, 0.005, x)
; var o = SinOsc.ar(40, 0) * n1
; Out.ar(0, LeakDC.ar(f + o, 0.995).clip2(0.75)) }.play

(let* ((n0 (LFNoise0 ar (mce2 20 40)))
       (n1 (LFNoise0 ar (mce2 5 10)))
       (x (MouseX kr 0.012 0.19 1 0.1))
       (f (Formlet (Blip ar 10 12) (MulAdd n0 43 700) 0.005 x))
       (o (Mul (SinOsc ar 40 0) n1)))
  (audition (Out 0 (Clip2 (Add f o) 1))))
