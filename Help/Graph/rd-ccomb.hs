-- ccomb (rd, 2006-10-07)
uid_st_eval (do
  let lwr = 48
      flwr = midiCPS lwr
      spart t = do n <- fmap (range lwr 72.0) (lfNoise2M KR 0.1)
                   e <- fmap (decay2 t 0.01) (tRandM 0.05 0.75 t)
                   x <- fmap (* e) (whiteNoiseM AR)
                   m <- lfNoise2M KR 0.1
                   let f = lag (midiCPS n) 0.25
                       m' = range 1 8 m
                   return (combC x (recip flwr) (recip f) m')
  t <- dustM KR (mce2 0.75 0.35)
  return . (* 0.01) . sum =<< sequence (replicate 12 (spart t)))
