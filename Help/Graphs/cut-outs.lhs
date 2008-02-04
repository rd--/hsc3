cut-outs (rd)

> let { t = impulse AR 22 0 * (sinOsc KR 0.5 0 + 1)
>     ; x = mouseX KR 0.005 0.12 Exponential 0.1
>     ; y = mouseY KR 0.01 0.52 Exponential 0.1 
>     ; n = do { n1 <- lfNoise0 KR 2
>              ; n2 <- coinGate (0.05 + n1 + y * 0.4 + t * 0.5) (t * 0.5)
>              ; n3 <- tExpRand (mce2 500 900) 1600 t
>              ; return (ringz n2 n3 x) } }
> in do { s <- liftM sum (replicateM 3 n)
>       ; b <- tRand 0 1 =<< dust KR 8
>       ; audition (mrg [out 0 b, out 0 (clip2 s (in' 1 KR 0) * 0.25)]) }

{ var t = Impulse.ar(22, 0) * (SinOsc.kr(0.5, 0) + 1)
; var x = MouseX.kr(0.005, 0.12, 'exponential', 0.1)
; var y = MouseY.kr(0.01, 0.52, 'exponential', 0.1)
; var n = { var n1 = LFNoise0.kr(2)
          ; var n2 = CoinGate.ar((0.05 + n1 + y) * (0.4 + t) * 0.5, t * 0.5)
          ; var n3 = TExpRand.ar([500, 900], 1600, t)
          ; Ringz.ar(n2, n3, x) }
; var s = Mix.fill(3, n)
; var b = TRand.kr(0, 1, Dust.kr(8))
; Out.kr(0, b)
; Out.ar(0, s.clip2(In.kr(0, 1)) * 0.25) }.play
