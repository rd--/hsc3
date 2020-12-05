-- cut-outs (rd, 2006-09-11)
uid_st_eval (do
  let t = impulse AR 22 0 * (sinOsc KR 0.5 0 + 1)
      x = mouseX KR 0.005 0.12 Exponential 0.1
      y = mouseY KR 0.01 0.52 Exponential 0.1
      n = do n1 <- lfNoise0M KR 2
             n2 <- coinGateM (0.05 + n1 + y * 0.4 + t * 0.5) (t * 0.5)
             n3 <- tExpRandM (mce2 500 900) 1600 t
             return (ringz n2 n3 x)
  s <- fmap sum (sequence (replicate 3 n))
  b <- tRandM 0 1 =<< dustM KR 8
  return (mrg [clip2 s (in' 1 KR 0) * 0.25,out 0 b]))
