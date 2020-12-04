-- cymbalism (jmcc) #2 ; texture=overlap,3,6,6,inf
uid_st_eval (do
  let p = replicate 15
  f1 <- randM 500 2500
  f2 <- randM 0 8000
  let y = do f <- sequence (p (randM f1 (f1 + f2)))
             rt <- sequence (p (randM 1 5))
             return (klankSpec f (p 1) rt)
  z <- clone 2 y
  n <- fmap (* 0.03) (whiteNoiseM AR)
  tf <- randM 0.5 3.5
  let t = impulse AR tf 0
      s = decay t 0.004 * n
  return (klank s 1 0 1 (mceTranspose z)))
