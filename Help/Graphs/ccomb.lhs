ccomb (rd)

> let { rng l r i = linLin i (-1) 1 l r
>     ; lwr = 48
>     ; flwr = midiCPS lwr
>     ; spart t = do { n <- liftM (rng lwr 72.0) (lfNoise2 KR 0.1)
>                    ; e <- liftM (decay2 t 0.01) (tRand 0.05 0.75 t)
>                    ; x <- liftM (* e) (whiteNoise AR)
>                    ; m <- lfNoise2 KR 0.1
>                    ; let f = lag (midiCPS n) 0.25
>                      in return (combC x (recip flwr) (recip f) (rng 1 8 m)) } }
> in do { t <- dust KR (mce2 0.75 0.35)
>       ; audition . (out 0) . (* 0.1) . sum =<< replicateM 12 (spart t) }

{ var lwr = 48
; var flwr = lwr.midicps
; var spart = { arg t
              ; { var n = LFNoise2.kr(0.1).range(lwr, 72.0)
                ; var e = Decay2.kr(t, 0.01, TRand.kr(0.05, 0.75, t))
                ; var x = WhiteNoise.ar() * e
                ; var m = LFNoise2.kr(0.1)
                ; var f = Lag.kr(n.midicps, 0.25)
                ; CombC.ar(x, flwr.reciprocal, f.reciprocal, m.range(1, 8)) } }
; var t = Dust.kr([0.75, 0.35])
; Out.ar(0, Mix.fill(12, spart.value(t)) * 0.1) }.play

(let* ((rng (lambda (u l r) (LinLin u -1 1 l r)))
       (lwr 48)
       (flwr (midicps 48))
       (spart (lambda (t)
		(let* ((n (rng (LFNoise2 kr 0.1) lwr 72))
		       (e (Decay2 t 0.01 (TRand 0.05 0.75 t)))
		       (x (Mul (WhiteNoise ar) e))
		       (m (LFNoise2 kr 0.1))
		       (f (Lag (MIDICPS n) 0.25)))
		  (CombC x (Recip flwr) (Recip f) (rng m 1.0 8.0)))))
       (t (Dust ar (Mce 0.9 0.8))))
  (audition (Out 0 (mix/fill 7 (lambda (_) (Mul (spart t) 0.1))))))
