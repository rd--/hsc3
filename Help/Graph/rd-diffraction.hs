-- diffraction (rd, 2006-09-09)
uid_st_eval (do
  let diffraction_p = do
        let x = mouseX KR 0.001 0.02 Exponential 0.1
            y = mouseY KR 120 400 Exponential 0.1
        f <- fmap (* mce2 32 64) (lfNoise0M KR 4)
        w <- fmap (* x) (lfNoise0M KR 32)
        z <- fmap (* 0.1) (lfNoise0M KR 2)
        m <- lfNoise0M KR 6
        let s = pulse AR f w
        return (resonz s (y + z) (m * 0.4 + 0.8) * 0.5)
      diffraction_q = do
        n <- lfNoise0M KR 128
        s <- diffraction_p
        return (combN s 0.2 (n * 0.1 + 0.1) 3)
      diffraction_r =
        let x = mouseX KR 0.75 1.25 Exponential 0.1
            y = mouseY KR 0.25 1 Exponential 0.1
            f :: UId m => Int -> m UGen
            f _ = do fr <- fmap (* x) (randM 50 59)
                     am <- fmap (* y) (randM 0.04 0.16)
                     return (sinOsc AR fr 0 * am)
        in liftM2 mce2 (mixFillM 16 f) (mixFillM 12 f)
  fmap sum (sequence [diffraction_p,diffraction_q,diffraction_r]))
