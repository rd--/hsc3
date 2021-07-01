-- cut-outs (rd, 2006-09-11)
let t = impulse ar 22 0 * (sinOsc kr 0.5 0 + 1)
    x = mouseX kr 0.005 0.12 Exponential 0.1
    y = mouseY kr 0.01 0.52 Exponential 0.1
    n _ = ringz (coinGate (0.05 + lfNoise0 kr 2 + y * 0.4 + t * 0.5) (t * 0.5)) (tExpRand (mce2 500 900) 1600 t) x
    s = mixFill 3 n
    b = tRand 0 1 (dust kr 8)
in mrg [clip2 s (in' 1 kr 0) * 0.25,out 0 b]

-- cut-outs (rd, 2006-09-11) ; monad
uid_st_eval (do
  let t = impulse ar 22 0 * (sinOsc kr 0.5 0 + 1)
      x = mouseX kr 0.005 0.12 Exponential 0.1
      y = mouseY kr 0.01 0.52 Exponential 0.1
      n = do n1 <- lfNoise0M kr 2
             n2 <- coinGateM (0.05 + n1 + y * 0.4 + t * 0.5) (t * 0.5)
             n3 <- tExpRandM (mce2 500 900) 1600 t
             return (ringz n2 n3 x)
  s <- fmap sum (sequence (replicate 3 n))
  b <- tRandM 0 1 =<< dustM kr 8
  return (mrg [clip2 s (in' 1 kr 0) * 0.25,out 0 b]))
