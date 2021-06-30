-- plucked strings (jmcc)
let dt _ = 1 / midiCPS (floorE (rand 60 90))
    i _ = let s0 = impulse ar (rand 2 2.2) 0 * 0.3
              s1 = dust ar 0.5 * 0.3
              s2 = impulse ar (sinOsc kr (rand 0.05 0.15) (rand 0 (pi * 2)) * 5 + 5.2) 0 * 0.3
          in select (iRand 0 2) (mce [s0,s1,s2])
    s n = let dt' = dt n
              t = decay (i n) 0.1 * pinkNoise ar * 0.1
          in pan2 (combL t dt' dt' 4) (rand2 1) 1
in mixFill 5 s

-- plucked strings (jmcc) ; monad
uid_st_eval (do
  let dt = do r0 <- randM 60 90
              return (1 / midiCPS (floorE r0))
      i = do r0 <- randM 2 2.2
             n0 <- dustM ar 0.5
             r1 <- randM 0.05 0.15
             r2 <- randM 0 (pi * 2)
             r3 <- iRandM 0 2
             let s0 = impulse ar r0 0 * 0.3
                 s1 = n0 * 0.3
                 s2 = impulse ar (sinOsc kr r1 r2 * 5 + 5.2) 0 * 0.3
             return (select r3 (mce [s0,s1,s2]))
      s = do n0 <- pinkNoiseM ar
             r1 <- randM (-1) 1
             im <- i
             dt' <- dt
             let t = decay im 0.1 * n0 * 0.1
             return (pan2 (combL t dt' dt' 4) r1 1)
  fmap sum (sequence (replicate 5 s)))
