wind metals (jmcc)

> import Sound.SC3.Monadic

> main =
>   let n = 6
>   in do { base <- expRand 60 4000
>         ; range <- rand 500 8000
>         ; n0 <- clone 2 (brownNoise ar)
>         ; r0 <- expRand 0.125 0.5
>         ; n1 <- lfNoise1 kr r0
>         ; f <- sequence (replicate n (rand base (base + range)))
>         ; dt <- sequence (replicate n (rand 0.1 2))
>         ; let { exc = n0 * 0.007 * max 0 (n1 * 0.75 + 0.25)
>               ; k = klankSpec f (replicate n 1) dt
>               ; s = klank exc 1 0 1 k }
>           in audition (out 0 (softClip (s * 0.1))) }

{ var n = 6
; var base = ExpRand.new(60, 4000)
; var range = Rand.new(500.0, 8000.0)
; var n0 = BrownNoise.ar([0.007, 0.007])
; var r0 = ExpRand.new(0.125, 0.5)
; var n1 = LFNoise1.kr(r0, 0.75, 0.25)
; var exc = n0 * max(0, n1)
; var f = Array.fill(n, { Rand.new(0, range) + base })
; var dt = Array.fill(n, { Rand.new(0.1, 2.0) })
; var s = Klank.ar(`[f, nil, dt], exc)
; Out.ar(0, (s * 0.1).softclip) }.play
