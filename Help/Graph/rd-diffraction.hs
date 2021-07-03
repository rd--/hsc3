-- diffraction (rd, 2006-09-09)
let diffraction_p _ =
      let x = mouseX kr 0.001 0.02 Exponential 0.1
          y = mouseY kr 120 400 Exponential 0.1
          f = mce2 32 64 * lfNoise0 kr 4
          s = pulse ar f (x * lfNoise0 kr 32)
      in resonz s (y + 0.1 * lfNoise0 kr 2) (lfNoise0 kr 6 * 0.4 + 0.8) * 0.5
    diffraction_q = combN (diffraction_p ()) 0.2 (lfNoise0 kr 128 * 0.1 + 0.1) 3
    diffraction_r =
        let x = mouseX kr 0.75 1.25 Exponential 0.1
            y = mouseY kr 0.25 1 Exponential 0.1
            f _ = sinOsc ar (x * rand 50 59) 0 * y * rand 0.04 0.16
        in mce2 (mixFill 16 f) (mixFill 12 f)
in sum [diffraction_p (),diffraction_q,diffraction_r]

-- diffraction (rd, 2006-09-09) ; monad
uid_st_eval (do
  let diffraction_p = do
        let x = mouseX kr 0.001 0.02 Exponential 0.1
            y = mouseY kr 120 400 Exponential 0.1
        f <- fmap (* mce2 32 64) (lfNoise0M kr 4)
        w <- fmap (* x) (lfNoise0M kr 32)
        z <- fmap (* 0.1) (lfNoise0M kr 2)
        m <- lfNoise0M kr 6
        let s = pulse ar f w
        return (resonz s (y + z) (m * 0.4 + 0.8) * 0.5)
      diffraction_q = do
        n <- lfNoise0M kr 128
        s <- diffraction_p
        return (combN s 0.2 (n * 0.1 + 0.1) 3)
      diffraction_r =
        let x = mouseX kr 0.75 1.25 Exponential 0.1
            y = mouseY kr 0.25 1 Exponential 0.1
            f _ = do fr <- fmap (* x) (randM 50 59)
                     am <- fmap (* y) (randM 0.04 0.16)
                     return (sinOsc ar fr 0 * am)
        in liftM2 mce2 (mixFillM 16 f) (mixFillM 12 f)
  fmap sum (sequence [diffraction_p,diffraction_q,diffraction_r]))
