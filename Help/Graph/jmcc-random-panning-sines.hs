-- random panning sines (jmcc) #4 ; texture=overlap,8,8,2,inf
let nd _ = pan2 (fSinOsc ar (linRand 80 2000 0) 0) (lfNoise1 kr (rand 0.8 1.2)) (lfNoise1 kr (rand 0.82 0.98))
in mixFill 8 nd * (0.4 / 8)

-- random panning sines (jmcc) #4 ; texture=overlap,8,8,2,inf ; monad
uid_st_eval (do
  let nd = do r0 <- linRandM 80 2000 0
              let o = fSinOsc ar r0 0
              l <- lfNoise1M kr =<< randM 0.8 1.2
              a <- lfNoise1M kr =<< randM 0.82 0.98
              return (pan2 o l a)
  r <- replicateM 8 nd
  return (mix (mce r) * (0.4 / 8)))
