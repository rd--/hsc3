-- discretion (rd, 2006-10-15)
let mkls bp t = envGen kr 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
    part _ = let t = 21
                 f = mkls [(0,X.rRandN 2 50 55),(0.33,X.rRandN 2 50 65),(1,X.rRandN 2 50 55)] t
                 a = mkls [(0,0),(0.33,X.rRandN 2 0.01 0.035),(1,0)] t
             in (saw ar f * a)
in mixFill 8 part

-- discretion (rd, 2006-10-15) ; id
let mkls bp t = envGen kr 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
    part z _ = let f1 = X.rRandNId 2 (z,'α') 50 55
                   f2 = X.rRandNId 2 (z,'β') 50 65
                   f3 = X.rRandNId 2 (z,'γ') 50 55
                   a = X.rRandNId 2 (z,'δ') 0.01 0.035
                   t = 21
                   f_ = mkls [(0,f1),(0.33,f2),(1,f3)] t
                   a_ = mkls [(0,0),(0.33,a),(1,0)] t
               in (saw ar f_ * a_)
in mixFill_z 'ε' 8 part

-- discretion (rd, 2006-10-15) ; monad
uid_st_eval (do
  let clone n = fmap mce . replicateM n
      mkls bp t = envGen kr 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
      part = do f1 <- clone 2 (randM 50 55)
                f2 <- clone 2 (randM 50 65)
                f3 <- clone 2 (randM 50 55)
                a <- clone 2 (randM 0.01 0.035)
                let t = 21
                    f_ = mkls [(0,f1),(0.33,f2),(1,f3)] t
                    a_ = mkls [(0,0),(0.33,a),(1,0)] t
                return (saw ar f_ * a_)
  fmap mix (clone 8 part))
