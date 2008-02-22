discretion (rd)

> let { mkls bp t = envGen KR 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
>     ; part = do { f1 <- clone 2 (rand 50 55)
>                 ; f2 <- clone 2 (rand 50 65)
>                 ; f3 <- clone 2 (rand 50 55)
>                 ; a <- clone 2 (rand 0.01 0.035)
>                 ; let { t = 21
>                       ; f_ = mkls [(0, f1), (0.33, f2), (1, f3)] t
>                       ; a_ = mkls [(0, 0), (0.33, a), (1, 0)] t }
>                   in return (saw AR f_ * a_) } }
> in audition . out 0 . mix =<< clone 8 part

(let* ((mkls (lambda (bp t)
	       (EnvGen kr 1 1 0 1 removeSynth (env/bp bp t 1))))
       (part (lambda (_)
	       (let* ((f1 (clone 2 (Rand 50 55)))
		      (f2 (clone 2 (Rand 50 65)))
		      (f3 (clone 2 (Rand 50 55)))
		      (a (clone 2 (Rand 0.01 0.035)))
		      (t 21)
		      (f_ (mkls (list 0.0 f1 0.33 f2 1.0 f3) t))
		      (a_ (mkls (list 0 0 0.33 a 1 0) t)))
		 (Mul (Saw ar f_) a_)))))
  (audition (Out 0 (mix/fill 8 part))))
