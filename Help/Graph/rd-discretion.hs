-- discretion (rd, 2006-10-15)
let mkls bp t = envGen KR 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
    part = let f1 = X.rRandN 2 'α' 50 55
               f2 = X.rRandN 2 'β' 50 65
               f3 = X.rRandN 2 'γ' 50 55
               a = X.rRandN 2 'δ' 0.01 0.035
               t = 21
               f_ = mkls [(0,f1),(0.33,f2),(1,f3)] t
               a_ = mkls [(0,0),(0.33,a),(1,0)] t
           in (saw AR f_ * a_)
in mix (Protect.uclone_all 'ε' 8 part)

-- discretion (rd, 2006-10-15)
uid_st_eval (do
  let mkls bp t = envGen KR 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
      part = do f1 <- clone 2 (randM 50 55)
                f2 <- clone 2 (randM 50 65)
                f3 <- clone 2 (randM 50 55)
                a <- clone 2 (randM 0.01 0.035)
                let t = 21
                    f_ = mkls [(0,f1),(0.33,f2),(1,f3)] t
                    a_ = mkls [(0,0),(0.33,a),(1,0)] t
                return (saw AR f_ * a_)
  fmap mix (clone 8 part))
