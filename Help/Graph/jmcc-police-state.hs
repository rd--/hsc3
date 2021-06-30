-- police state (jmcc) #2
let nd _ = let f = sinOsc kr (rand 0.02 0.12) (rand 0 (pi * 2)) * rand 0 600 + rand 700 1300
           in pan2 (sinOsc ar f 0 * lfNoise2 ar (rand 80 120) * 0.1) (rand2 1) 1
    e = lfNoise2 ar (mceFill 2 (\_ -> lfNoise2 kr 0.4) * 90 + 620) * lfNoise2 kr (mce2 0.3 0.301) * 0.15 + 0.18
in combL (mixFill 4 nd + e) 0.3 0.3 3

-- police state (jmcc) #2 ; monad
uid_st_eval (do
  let nd = do r0 <- randM 0.02 0.12
              r1 <- randM 0 (pi * 2)
              r2 <- randM 0 600
              r3 <- randM 700 1300
              r4 <- randM (-1) 1
              r5 <- randM 80 120
              n0 <- lfNoise2M ar r5
              let f = sinOsc kr r0 r1 * r2 + r3
              return (pan2 (sinOsc ar f 0 * n0 * 0.1) r4 1)
  ns <- replicateM 4 nd
  n0 <- replicateM 2 (lfNoise2M kr 0.4)
  n1 <- lfNoise2M ar (mce n0 * 90 + 620)
  n2 <- lfNoise2M kr (mce2 0.3 0.301)
  let e = n1 * (n2 * 0.15 + 0.18)
  return (combL (mix (mce ns) + e) 0.3 0.3 3))
