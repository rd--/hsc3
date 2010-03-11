lg-timed (rd)

> import Sound.SC3.Monadic

> main =
>   let { timed r y p =
>         do { d0 <- dser r p
>            ; d1 <- dcons 0 d0
>            ; d2 <- dser r y
>            ; let t = tDuty AR d1 0 RemoveSynth d2 1
>              in return (latch t t) }
>       ; lg u = return (lag u 0.03)
>       ; n = mce [52, 76, 66, 67, 68, 69]
>       ; a = mce [0.35, 0.15, 0.04, 0.05, 0.16, 0.07]
>       ; d = mce [0.1, 0.5, 0.09, 0.08, 0.07, 0.3]
>       ; x = mouseX KR 0.5 1.25 Linear 0.2 }
>   in do { tn <- lg =<< timed dinf n (d * x)
>         ; ta <- lg =<< timed dinf a (d * x)
>         ; audition (out 0 (sinOsc AR (midiCPS tn) 0 * ta)) }

{ var dcons = { arg x, xs
              ; var i = Dseq.new([0, 1], 1)
              ; var a = Dseq.new([x, xs], 1)
              ; Dswitch.new(a, i) }
; var timed = { arg r, y, p
              ; var d0 = Dser.new(p, r)
              ; var d1 = dcons.value(0, d0)
              ; var d2 = Dser.new(y, r)
              ; var t = TDuty.ar(d1, 0, d2, 2, 1)
              ; Latch.ar(t, t) }
; var lg = { arg u
           ; Lag.ar(u, 0.03) }
; var n = [52, 76, 66, 67, 68, 69]
; var a = [0.35, 0.15, 0.04, 0.05, 0.16, 0.07]
; var d = [0.1, 0.5, 0.09, 0.08, 0.07, 0.3]
; var x = MouseX.kr(0.5, 1.25, 'linear', 0.2)
; var tn = lg.value(timed.value(inf, n, d * x))
; var ta = lg.value(timed.value(inf, a, d * x))
; Out.ar(0, SinOsc.ar(tn.midicps, 0) * ta) }.play
