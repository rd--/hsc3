reverberated sine percussion (jmcc)

> import Control.Monad
> import Sound.SC3
> import qualified Sound.SC3.UGen.Monadic as M

> let { d = 6
>     ; c = 5
>     ; a = 4
>     ; s_ = do { n <- M.dust ar (2 / constant d)
>               ; r <- M.rand 0 3000
>               ; return (resonz (n * 50) (200 + r) 0.003) }
>     ; x_ i = do { r <- clone 2 (M.rand 0 0.05)
>                 ; return (allpassN i 0.05 r 1) } 
>     ; chain n f = foldl (>=>) return (replicate n f) }
> in do { s <- fmap sum (sequence (replicate d s_))
>       ; y <- let z = delayN s 0.048 0.48
>              in do { r <- clone c (M.rand 0 0.1)
>                    ; n <- M.lfNoise1 kr r
>                    ; return (mix (combL z 0.1 (n * 0.04 + 0.05) 15)) }
>       ; x <- chain a x_ y
>       ; audition (out 0 (s + x * 0.2)) }

{ var d = 6
; var c = 5
; var a = 4
; var s_ = { var n = Dust.ar(2 / d)
           ; var r = Rand.new(0, 3000)
           ; Resonz.ar(n * 50, 200 + r, 0.003) }
; var s = Mix.ar(Array.fill(d, s_))
; var z = DelayN.ar(s, 0.048)
; var y_ = LFNoise1.kr(Array.fill(c, { Rand.new(0, 0.1) }), 0.04, 0.05)
; var y = Mix.ar(CombL.ar(z, 0.1, y_, 15))
; var x = y
; var x_ = { var r = [Rand.new(0, 0.05), Rand.new(0, 0.05)]
           ; x = AllpassN.ar(x, 0.050, r, 1) }
; a.do(x_)
; Out.ar(0, s + (0.2 * x)) }.play
