-- random panning sines (jmcc) #4 ; texture=overlap,8,8,2,inf
uid_st_eval (do
  let nd = do r0 <- linRandM 80 2000 0
              let o = fSinOsc AR r0 0
              l <- lfNoise1M KR =<< randM 0.8 1.2
              a <- lfNoise1M KR =<< randM 0.82 0.98
              return (pan2 o l a)
  r <- clone 8 nd
  return (mix r * (0.4 / 8)))
