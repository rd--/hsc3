-- plucked strings (jmcc)
uid_st_eval (do
  let dt = do r0 <- randM 60 90
              return (1 / midiCPS (floorE r0))
      i = do r0 <- randM 2 2.2
             n0 <- dustM AR 0.5
             r1 <- randM 0.05 0.15
             r2 <- randM 0 (pi * 2)
             r3 <- iRandM 0 2
             let s0 = impulse AR r0 0 * 0.3
                 s1 = n0 * 0.3
                 s2 = impulse AR (sinOsc KR r1 r2 * 5 + 5.2) 0 * 0.3
             return (select r3 (mce [s0,s1,s2]))
      s = do n0 <- pinkNoiseM AR
             r1 <- randM (-1) 1
             im <- i
             dt' <- dt
             let t = decay im 0.1 * n0 * 0.1
             return (pan2 (combL t dt' dt' 4) r1 1)
  fmap sum (sequence (replicate 5 s)))
