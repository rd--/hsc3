wind metals (jmcc)

> let n = 6
> in do { base <- expRand 60 4000
>       ; range <- rand 500 8000
>       ; n0 <- clone 2 (brownNoise AR)
>       ; r0 <- expRand 0.125 0.5
>       ; n1 <- lfNoise1 KR r0
>       ; f <- replicateM n (rand base (base + range))
>       ; dt <- replicateM n (rand 0.1 2)
>       ; let { exc = n0 * 0.007 * max 0 (n1 * 0.75 + 0.25)
>             ; k = klankSpec f (replicate n 1) dt
>             ; s = klank exc 1 0 1 k }
>         in audition (out 0 (softClip (s * 0.1))) }

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

(let* ((n 6)
       (base (ExpRand 60 4000))
       (range (Rand 500 8000))
       (n0 (clone 2 (BrownNoise ar)))
       (r0 (ExpRand 0.125 0.5))
       (n1 (LFNoise1 kr r0))
       (f (replicate-m n (Rand base (Add base range))))
       (dt (replicate-m n (Rand 0.1 2)))
       (exc (Mul3 n0 0.007 (Max 0 (MulAdd n1 0.75 0.25))))
       (k (klank-data f (replicate n 1) dt))
       (s (Klank exc 1 0 1 k)))
  (audition (Out 0 (SoftClip (Mul s 0.1)))))
