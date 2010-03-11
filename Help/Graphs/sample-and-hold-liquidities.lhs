sample and hold liquidities (jmcc)

> import Sound.SC3.ID

> main =
>   let { r = mouseX kr 1 200 Exponential 0.1
>       ; t = recip r
>       ; c = impulse kr r 0.4
>       ; cf = mouseY kr 100 8000 Exponential 0.1
>       ; f = latch (whiteNoise 'α' kr * cf * 0.5 + cf) c
>       ; p = latch (whiteNoise 'β' kr) c
>       ; i = pan2 (sinOsc ar f 0 * decay2 c (t * 0.1) (t * 0.9)) p 1 }
>   in audition (out 0 (combN i 0.3 0.3 2))

{ var r = MouseX.kr(1, 200, 'exponential', 0.1)
; var t = r.reciprocal
; var c = Impulse.kr(r, 0.4)
; var cf = MouseY.kr(100, 8000, 'exponential', 0.1)
; var f = Latch.kr(WhiteNoise.kr * cf * 0.5 + cf, c)
; var p = Latch.kr(WhiteNoise.kr, c)
; var i = Pan2.ar(SinOsc.ar(f, 0, Decay2.kr(c, 0.1 * t, 0.9 * t)), p, 1)
; Out.ar(0, CombN.ar(i, 0.3, 0.3, 2)) }.play
