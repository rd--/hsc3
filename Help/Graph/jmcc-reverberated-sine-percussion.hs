-- reverberated sine percussion (jmcc) #3
let s = mixFill 6 (\_ -> resonz (dust ar (2 / 6) * 50) (200 + rand 0 3000) 0.003)
    y = mix (combL (delayN s 0.048 0.48) 0.1 (lfNoise1 kr (X.randN 5 0 0.1) * 0.04 + 0.05) 15)
    x = iter 4 (\i -> allpassN i 0.05 (X.randN 2 0 0.05) 1) y
in s + x * 0.2

-- reverberated sine percussion (jmcc) #3 ; monad
uid_st_eval (do
  let d = 6
      c = 5
      a = 4
      s_ = do n <- dustM ar (2 / constant d)
              r <- randM 0 3000
              return (resonz (n * 50) (200 + r) 0.003)
      x_ i = do r <- X.randNM 2 0 0.05
                return (allpassN i 0.05 r 1)
  s <- fmap sum (sequence (replicate d s_))
  y <- do let z = delayN s 0.048 0.48
          r <- X.randNM c 0 0.1
          n <- lfNoise1M kr r
          return (mix (combL z 0.1 (n * 0.04 + 0.05) 15))
  x <- chainM a x_ y
  return (s + x * 0.2))
