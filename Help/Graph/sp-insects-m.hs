-- insects (sp)
let insect _ =
      let a = sinOsc kr (rand 1 10) 0 * 0.5 + 0.5
          o = sinOsc ar (rand 2000 3000 + lfNoise2 kr (rand 0.05 0.1 * 50 + 50)) (rand 0.05 0.1) * 0.005 * a
      in pan2 o (lfNoise2 kr (rand 0.05 0.1)) 1
in mixFill 60 insect

-- insects (sp) ; monad
let insect_m = do
      r1 <- randM 2000 3000
      r2 <- randM 0.05 0.1
      n1 <- lfNoise2M kr (r2 * 50 + 50)
      r4 <- randM 1 10
      r5 <- randM 0.05 0.1
      n2 <- lfNoise2M kr r5
      let a = sinOsc kr r4 0 * 0.5 + 0.5
          o = sinOsc ar (r1 + n1) r2 * 0.005 * a
      return (pan2 o n2 1)
in uid_st_eval (fmap sum (replicateM 60 insect_m))
