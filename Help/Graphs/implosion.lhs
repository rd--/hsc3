implosion (rd)

> let { mkls bp t = let e = envCoord bp t 1 EnvLin
>                   in envGen KR 1 1 0 1 RemoveSynth e
>     ; mkrmp l r t = mkls [(0, l), (1, r)] t
>     ; wrp i l r = linLin i (-1) 1 l r
>     ; pmr_n rt l0 l1 r0 r1 d = let { le = mkrmp l0 r0 d
>                                    ; re = mkrmp l1 r1 d }
>                                in do { n <- whiteNoise rt
>                                      ; return (wrp n le re) } }
> in do { n0 <- rand (-1) 0
>       ; n1 <- rand 0 1
>       ; d  <- rand 7.5 13.5
>       ; f0 <- rand 10990 16220
>       ; f1 <- rand  9440 19550
>       ; f <- pmr_n AR 440 f0 f1 f1 d
>       ; l <- pmr_n KR n0 n1 0 0 d
>       ; a <- pmr_n KR 0.1 0.6 0 0 d
>       ; audition (out 0 (pan2 (saw AR f) l a)) }

(let* ((mkls (lambda (bp t)
	       (let ((c (replicate (/ (length bp) 2) 1)))
		 (EnvGen kr 1 1 0 1 removeSynth (env/bp bp t 1 c)))))
       (mkrmp (lambda (l r t)
		(mkls (list 0 l 1 r) t)))
       (wrp (lambda (i l r)
	      (let ((m (FDiv (Sub r l) 2)))
		(MulAdd i m (Add l m)))))
       (pmr/n (lambda (rt l0 l1 r0 r1 d)
		(let ((le (mkrmp l0 r0 d))
		      (re (mkrmp l1 r1 d)))
		  (wrp (WhiteNoise rt) le re))))
       (d (Rand 7.5 13.5))
       (f0 (Rand 10990 16220))
       (f1 (Rand  9440 19550))
       (f (pmr/n ar 440 f0 f1 f1 d))
       (l (pmr/n kr (Rand -1 0) (Rand 0 1) 0 0 d))
       (a (pmr/n kr 0.1 0.6 0 0 d)))
  (audition (Out 0 (Pan2 (Saw ar f) l a))))
