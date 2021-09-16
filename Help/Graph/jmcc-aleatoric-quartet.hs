-- aleatoric quartet (jmcc) #7
let base_mn = 66 -- control kr "note" 66
    amp = 0.07 -- control kr "amp" 0.07
    density = mouseX kr 0.01 1 Linear 0.1
    dmul = recip density * 0.5 * amp
    dadd = amp - dmul
    rapf i = allpassN i 0.05 (mce2 (rand 0 0.05) (rand 0 0.05)) 1
    mk_f _ = let m = lfNoise0 kr (lchoose [1, 0.5, 0.25]) * 7 + base_mn + rand (-30) 30
             in midiCps (lag (roundTo m 1) 0.2)
    mk_s n = let x = pinkNoise ar * max 0 (lfNoise1 kr 8 * dmul + dadd)
             in pan2 (combL x 0.02 (recip (mk_f n)) 3) (rand2 1) 1
    g = iter 5 rapf (mixFill 4 mk_s)
in leakDC g 0.995

-- aleatoric quartet (jmcc) #7 ; monadic
let aleatoric_quartet_m = do
      let base_mn = 66 -- control kr "note" 66
          amp = 0.07 -- control kr "amp" 0.07
          density = mouseX kr 0.01 1 Linear 0.1
          dmul = recip density * 0.5 * amp
          dadd = amp - dmul
          rapf i = do r <- replicateM 2 (randM 0 0.05)
                      return (allpassN i 0.05 (mce r) 1)
          mk_f = do r0 <- lchooseM [1, 0.5, 0.25]
                    r1 <- randM (-30) 30
                    n0 <- lfNoise0M kr r0
                    let m = n0 * 7 + base_mn + r1
                        m' = lag (roundTo m 1) 0.2
                    return (midiCps m')
          mk_s = do f <- fmap recip mk_f
                    r <- randM (-1) 1
                    x <- do n0 <- pinkNoiseM ar
                            n1 <- lfNoise1M kr 8
                            return (n0 * max 0 (n1 * dmul + dadd))
                    return (pan2 (combL x 0.02 f 3) r 1)
      g <- chainM 5 rapf =<< fmap sum (sequence (replicate 4 mk_s))
      return (leakDC g 0.995)
in uid_st_eval aleatoric_quartet_m
