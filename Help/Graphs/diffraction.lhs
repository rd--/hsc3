diffraction (rd)

> import Control.Monad
> import Sound.SC3.Monadic

> main =
>   let { p = let { x = mouseX kr 0.001 0.02 Exponential 0.1
>                 ; y = mouseY kr 120 400 Exponential 0.1 }
>             in do { f <- fmap (* mce2 32 64) (lfNoise0 kr 4)
>                   ; w <- fmap (* x) (lfNoise0 kr 32)
>                   ; z <- fmap (* 0.1) (lfNoise0 kr 2)
>                   ; m <- lfNoise0 kr 6
>                   ; let s = pulse ar f w
>                     in return (resonz s (y + z) (m * 0.4 + 0.8) * 0.5) }
>       ; q = do { n <- lfNoise0 kr 128
>                ; s <- p
>                ; return (combN s 0.2 (n * 0.1 + 0.1) 3) }
>       ; r = let { x = mouseX kr 0.75 1.25 Exponential 0.1
>                 ; y = mouseY kr 0.25 1 Exponential 0.1
>                 ; f _ = do { fr <- fmap (* x) (rand 50 59)
>                            ; am <- fmap (* y) (rand 0.04 0.16)
>                            ; return (sinOsc ar fr 0 * am) } }
>             in liftM2 mce2 (mixFillM 16 f) (mixFillM 12 f) }
>   in audition . (out 0) . sum =<< sequence [p, q, r]

{ var x = MouseX.kr(0.001, 0.02, 'exponential', 0.1)
; var y = MouseY.kr(120, 400, 'exponential', 0.1)
; var p = { var f = LFNoise0.kr(4) * [32, 64]
	      ; var w = LFNoise0.kr(32) * x
	      ; var z = LFNoise0.kr(2) * 0.1
          ; var m = LFNoise0.kr(6)
	      ; var s = Pulse.ar(f, w)
          ; Resonz.ar(s, y + z, (m * 0.4) + 0.8) * 0.5 }
; var q = { var n = LFNoise0.kr(128)
	      ; CombN.ar(p.value, 0.2, (n * 0.1) + 0.1, 3) }
; var r = { var x1 = MouseX.kr(0.75, 1.25, 'exponential', 0.1)
          ; var y1 = MouseY.kr(0.25, 1, 'exponential', 0.1)
	      ; var f = { var fr = Rand(50, 59) * x1
	                ; var am = Rand(0.04, 0.16) * y1
                    ; SinOsc.ar(fr, 0) * am }
          ; [Mix.fill(16, f), Mix.fill(12, f)] }
; Out.ar(0, p.value + q.value + r.value) }.play
