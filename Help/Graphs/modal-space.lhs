modal space (jmcc)

> let { b = 0
>     ; p = [0, 2, 3.2, 5, 7, 9, 10] 
>     ; x = mouseX KR 0 15 Linear 0.1
>     ; k = degreeToKey 0 x 12
>     ; f n r = let { o = sinOsc AR (midiCPS (r + k + n * 0.04)) 0 * 0.1
>                   ; t = lfPulse AR (midiCPS (mce2 48 55)) 0.15 0.5
>                   ; d = rlpf t (midiCPS (sinOsc KR 0.1 0 * 10 + r)) 0.1 * 0.1
>                   ; m = o + d }
>               in combN m 0.31 0.31 2 + m }
> in withSC3 (\fd -> do { async fd (b_alloc b (length p) 1)
>                       ; send fd (b_setn1 b 0 p)
>                       ; n <- clone 2 (lfNoise1 KR 3)
>                       ; play fd (out 0 ((f n 48 + f n 72) * 0.25)) })

{ var s = Server.default
; var b = 0
; var p = FloatArray[0, 2, 3.2, 5, 7, 9, 10]
; var n = LFNoise1.kr([3, 3])
; var x = MouseX.kr(0, 15, 0, 0.1)
; var k = DegreeToKey.kr(b, x, 12)
; var f = { arg r
          ; var o = SinOsc.ar((k + r + (n * 0.04)).midicps, 0) * 0.1
          ; var t = LFPulse.ar([48, 55].midicps, 0.15, 0.5)
          ; var d = RLPF.ar(t, SinOsc.kr(0.1, 0, 10, r).midicps, 0.1, 0.1)
          ; var m = o + d
          ; CombN.ar(m, 0.31, 0.31, 2) + m }
; s.sendMsg("/b_alloc", b, p.size, 1, ["/b_setn", b, 0, p].asRawOSC)
; Out.ar(0, (f.value(48) + f.value(72)) * 0.25) }.play
