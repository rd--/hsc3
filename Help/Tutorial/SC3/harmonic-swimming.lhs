harmonic swimming

> let { o h = do { r <- clone 2 (rand 2 8)
>                ; n <- lfNoise1 KR r
>                ; let { a = 0.08
>                      ; f = 50
>                      ; l = line KR 0 (- a) 60 DoNothing 
>                      ; e = max 0 (n * a + l) }
>                  in return (fSinOsc AR (f * h) 0 * e) } }
> in audition . out 0 . sum =<< mapM o [1..21]

{ var f = 50
; var p = 20
; var z = 0
; var offset = Line.kr(0, -0.02, 60)
; p.do({ 
    arg i
  ; var e = max(0, LFNoise1.kr(6 + [4.0.rand2, 4.0.rand2], 0.02, offset))
  ; z = FSinOsc.ar(f * (i+1), 0, e, z) })
; Out.ar(0, z) }.play
