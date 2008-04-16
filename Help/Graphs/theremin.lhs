theremin (jmcc)

> let { m = 7
>     ; detune = 0
>     ; x = mouseX KR 0 0.9 Linear 0.2
>     ; y = mouseY KR 4000 200 Exponential 0.8
>     ; f = y + detune
>     ; f' = f + f * sinOsc AR m 0 * 0.02
>     ; a = sinOsc AR f' 0 * x }
> in audition (out 0 (pan2 a 0 1))

{ var mod = 7
; var detune = 0
; var x = MouseX.kr(0, 0.9, 'linear', 0.2)
; var y = MouseY.kr(4000, 200, 'exponential', 0.8)
; var f = y + detune
; var f_ = f + (f * SinOsc.ar(mod, 0) * 0.02)
; var a = SinOsc.ar(f_, 0) * x
; Out.ar(0, (Pan2.ar(a, 0, 1))) }.play
