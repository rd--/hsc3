aleatoric quartet (jmcc)

> import Sound.SC3.Monadic

> main = 
>   let { base_mn = control KR "note" 66
>       ; amp = control KR "ampl" 0.07
>       ; density = mouseX kr 0.01 1 Linear 0.1
>       ; dmul = recip density * 0.5 * amp
>       ; dadd = amp - dmul
>       ; (>=>) f g = \x ->   f x >>= g
>       ; chain n f = foldl (>=>) return (replicate n f)
>       ; rapf i = do { r <- clone 2 (rand 0 0.05)
>                     ; return (allpassN i 0.05 r 1) }
>       ; mk_f = do { i0 <- iRand 0 2
>                   ; let r0 = select i0 (mce [1, 0.5, 0.25])
>                     in do { r1 <- rand (-30) 30
>                           ; n0 <- lfNoise0 kr r0
>                           ; let { m = n0 * 7 + base_mn + r1
>                                 ; m' = lag (roundE m 1) 0.2 }
>                             in return (midiCPS m) } }
>       ; mk_s = do { f <- fmap recip mk_f
>                   ; r <- rand (-1) 1
>                   ; x <- do { n0 <- pinkNoise ar
>                             ; n1 <- lfNoise1 kr 8
>                             ; return (n0 * max 0 (n1 * dmul + dadd)) }
>                   ; return (pan2 (combL x 0.02 f 3) r 1) } }
>   in do { g <- chain 5 rapf =<< fmap sum (sequence (replicate 4 mk_s))
>         ; audition (out 0 (leakDC g 0.995)) }

{ var amp = 0.07
; var density = MouseX.kr(0.01, 1, 'linear', 0.1)
; var dmul = density.reciprocal * 0.5 * amp
; var dadd = amp - dmul
; var rapf = { arg i
             ; var r = Array.fill(2, { Rand(0, 0.05) })
             ; AllpassN.ar(i, 0.05, r, 1) }
; var mk_f = { var i0 = IRand.new(0, 2)
             ; var r0 = Select.kr(i0, [1, 0.5, 0.25])
             ; var r1 = Rand.new(-30, 30)
             ; var n0 = LFNoise0.kr(r0)
             ; var m = Lag.kr((n0 * 7 + 66 + r1).round(1), 0.2)
             ; m.midicps }
; var mk_s = { var f = mk_f.value.reciprocal
             ; var r = Rand.new(-1, 1)
             ; var n0 = PinkNoise.ar()
             ; var n1 = LFNoise1.kr(8)
             ; var x = n0 * 0.max(n1 * dmul + dadd)
             ; Pan2.ar(CombL.ar(x, 0.02, f, 3), r, 1) }
; var g = Mix.fill(4, mk_s)
; 5.do({ g = rapf.value(g) })
; Out.ar(0, LeakDC.ar(g, 0.995)) }.play
