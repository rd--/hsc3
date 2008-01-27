reverberated sine percussion

> let { d = 6
>     ; c = 5
>     ; a = 4
>     ; s_ = do { n <- dust AR (2 / constant d)
>               ; r <- rand 0 3000
>               ; return (resonz (n * 50) (200 + r) 0.003) }
>     ; x_ i = do { r <- clone 2 (rand 0 0.05)
>                 ; return (allpassN i 0.05 r 1) } 
>     ; chain n f = foldl (>=>) return (replicate n f) }
> in do { s <- liftM sum (replicateM d s_)
>       ; y <- let z = delayN s 0.048 0.48
>              in do { r <- clone c (rand 0 0.1)
>                    ; n <- lfNoise1 KR r
>                    ; return (mix (combL z 0.1 (n * 0.04 + 0.05) 15)) }
>       ; x <- chain a x_ y
>       ; audition (out 0 (s + x * 0.2)) }

{ var d = 6
; var c = 5
; var a = 4
; var s_ = { var n = Dust.ar(2 / d)
           ; var r = 3000.0.rand
           ; Resonz.ar(n * 50, 200 + r, 0.003) }
; var s = Mix.ar(Array.fill(d, s_))
; var z = DelayN.ar(s, 0.048)
; var y_ = LFNoise1.kr(Array.fill(c, { 0.1.rand }), 0.04, 0.05)
; var y = Mix.ar(CombL.ar(z, 0.1, y_, 15))
; var x = y
; var x_ = { var r = [0.050.rand, 0.050.rand]
           ; x = AllpassN.ar(x, 0.050, r, 1) }
; a.do(x_)
; Out.ar(0, s + (0.2 * x)) }.play
