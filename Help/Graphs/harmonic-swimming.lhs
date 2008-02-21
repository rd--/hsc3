harmonic swimming (jmcc)

> let { a = 0.02
>     ; f = 50
>     ; p = 20
>     ; l = line KR 0 (- a) 60 DoNothing 
>     ; o h = do { r <- clone 2 (rand 2 8)
>                ; n <- lfNoise1 KR r
>                ; let e = max 0 (n * a + l)
>                  in return (fSinOsc AR (f * (h + 1)) 0 * e) } }
> in audition . out 0 . sum =<< mapM o [0..p]

{ var a = 0.02
; var f = 50
; var p = 20
; var l = Line.kr(0, a.neg, 60, 0)
; var o = { arg h
          ; var r = 6 + [Rand.new(-4, 4), Rand.new(-4, 4)]
          ; var n = LFNoise1.kr(r)
          ; var e = max(0, n * a + l)
          ; FSinOsc.ar(f * (h + 1), 0) * e }
; Out.ar(0, (0..p).collect(o).sum) }.play

(require (only-in srfi/1 iota))

(let* ((a 0.02)
       (f 50)
       (p 20)
       (z 0)
       (l (Line kr 0 (- a) 60 0))
       (o (lambda (h)
	    (let* ((r (dupn 2 (Rand 2 8)))
		   (n (LFNoise1 kr r))
		   (e (Max 0 (MulAdd n a l))))
	      (Mul (FSinOsc ar (* f (+ h 1)) 0) e)))))
  (audition (Out 0 (mix (make-mce (map o (iota p)))))))
