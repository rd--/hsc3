modal space (jmcc)

> import Sound.SC3.Monadic

> main =
>   let { b = 0
>       ; p = [0, 2, 3.2, 5, 7, 9, 10] 
>       ; x = mouseX kr 0 15 Linear 0.1
>       ; k = degreeToKey 0 x 12
>       ; c n r = let { o = sinOsc ar (midiCPS (r + k + n * 0.04)) 0 * 0.1
>                     ; t = lfPulse ar (midiCPS (mce2 48 55)) 0.15 0.5
>                     ; f = midiCPS (sinOsc kr 0.1 0 * 10 + r)
>                     ; d = rlpf t f 0.1 * 0.1
>                     ; m = o + d }
>                 in combN m 0.31 0.31 2 + m }
>   in withSC3 (\fd -> do { async fd (b_alloc b (length p) 1)
>                         ; send fd (b_setn1 b 0 p)
>                         ; n <- clone 2 (lfNoise1 kr 3)
>                         ; play fd (out 0 ((c n 48 + c n 72) * 0.25)) })

{ var s = Server.default
; var b = 0
; var p = FloatArray[0, 2, 3.2, 5, 7, 9, 10]
; var x = MouseX.kr(0, 15, 'linear', 0.1)
; var k = DegreeToKey.kr(b, x, 12)
; var c = { arg n, r
          ; var o = SinOsc.ar((r + k + (n * 0.04)).midicps, 0) * 0.1
          ; var t = LFPulse.ar([48, 55].midicps, 0.15, 0.5)
          ; var f = (SinOsc.kr(0.1, 0) * 10 + r).midicps
          ; var d = RLPF.ar(t, f, 0.1) * 0.1
          ; var m = o + d
          ; CombN.ar(m, 0.31, 0.31, 2) + m }
; var n = LFNoise1.kr([3, 3])
; var b_setn1 = { arg b, i, p
                ; ["/b_setn", b, i, p.size] ++ p }
; s.sendMsg("/b_alloc", b, p.size, 1, b_setn1.value(b, 0, p).asRawOSC)
; Out.ar(0, (c.value(n, 48) + c.value(n, 72)) * 0.25) }.play
