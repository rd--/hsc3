harmonic swimming (jmcc)

> let { a = 0.02
>     ; f = 50
>     ; p = 20
>     ; o h = do { r <- clone 2 (rand 2 8)
>                ; n <- lfNoise1 KR r
>                ; let { 
>                      ; l = line KR 0 (- a) 60 DoNothing 
>                      ; e = max 0 (n * a + l) }
>                  in return (fSinOsc AR (f * (h + 1)) 0 * e) } }
> in audition . out 0 . sum =<< mapM o [0..p]

{ var a = 0.02
; var f = 50
; var p = 20
; var z = 0
; var l = Line.kr(0, a.neg, 60, 0)
; var r = 6 + [4.0.rand2, 4.0.rand2]
; p.do({ 
    arg i
  ; var n = LFNoise1.kr(r)
  ; var e = max(0, n * a + l)
  ; z = FSinOsc.ar(f * (i + 1), 0, e, z) })
; Out.ar(0, z) }.play
