sample and hold liquidities (jmcc)

> let { wn = Sound.SC3.UGen.Base.whiteNoise
>     ; r = mouseX KR 1 200 Exponential 0.1
>     ; t = recip r
>     ; c = impulse KR r 0.4
>     ; cf = mouseY KR 100 8000 Exponential 0.1
>     ; f = latch (wn (uid 0) KR * cf * 0.5 + cf) c
>     ; p = latch (wn (uid 1) KR) c
>     ; i = pan2 (sinOsc AR f 0 * decay2 c (t * 0.1) (t * 0.9)) p 1 }
> in audition (out 0 (combN i 0.3 0.3 2))

{ var r = MouseX.kr(1, 200, 1)
; var t = r.reciprocal
; var c = Impulse.kr(r, 0.4)
; var cf = MouseY.kr(100, 8000, 1)
; var f = Latch.kr(WhiteNoise.kr(cf * 0.5, cf), c)
; var p = Latch.kr(WhiteNoise.kr, c)
; var i = Pan2.ar(SinOsc.ar(f, 0, Decay2.kr(c, 0.1 * t, 0.9 * t)), p, 1)
; Out.ar(0, CombN.ar(i, 0.3, 0.3, 2)) }.play

(let* ((r (MouseX kr 1 200 1 0.1))
       (t (Recip r))
       (c (Impulse kr r 0.4))
       (cf (MouseY kr 100 8000 1 0.1))
       (f (Latch (MulAdd (WhiteNoise kr) (Mul cf 0.5) cf) c))
       (p (Latch (WhiteNoise kr) c))
       (i (Pan2 (Mul (SinOsc ar f 0) (Decay2 c (Mul 0.1 t) (Mul 0.9 t))) p 1)))
  (audition (Out 0 (CombN i 0.3 0.3 2))))
