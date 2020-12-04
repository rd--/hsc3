-- aleatoric quartet (jmcc) #7
let aleatoric_quartet_m = do
      let base_mn = 66 -- control KR "note" 66
          amp = 0.07 -- control KR "amp" 0.07
          density = mouseX KR 0.01 1 Linear 0.1
          dmul = recip density * 0.5 * amp
          dadd = amp - dmul
          rapf i = do r <- clone 2 (randM 0 0.05)
                      return (allpassN i 0.05 r 1)
          mk_f = do r0 <- lchooseM [1, 0.5, 0.25]
                    r1 <- randM (-30) 30
                    n0 <- lfNoise0M KR r0
                    let m = n0 * 7 + base_mn + r1
                        m' = lag (roundTo m 1) 0.2
                    return (midiCPS m')
          mk_s = do f <- fmap recip mk_f
                    r <- randM (-1) 1
                    x <- do n0 <- pinkNoiseM AR
                            n1 <- lfNoise1M KR 8
                            return (n0 * max 0 (n1 * dmul + dadd))
                    return (pan2 (combL x 0.02 f 3) r 1)
      g <- chainM 5 rapf =<< fmap sum (sequence (replicate 4 mk_s))
      return (leakDC g 0.995)
in uid_st_eval aleatoric_quartet_m
