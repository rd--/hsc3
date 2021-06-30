-- birds (jmcc)
let node _ = let f = rand 94.0 102.0 + lag (lfSaw ar (7 + rand2 1.5) (rand 0 1) * rand 11 15) 0.1
                 a = lfPulse kr (1.0 / rand 12 15.6) (rand 0 1) 0.16 * 0.05
                 b = sinOsc ar (midiCPS f) (rand 0 1) * a
             in rotate2 b (silent 1) (rand2 1)
    apfr i = allpassL i 0.07 (rand 0 0.06) (rand 0.7 2.0)
    d = mixFill 6 node
    w = iter 12 apfr d
in d * 0.7 + w * 0.3

-- birds (jmcc) ; monad
uid_st_eval (do
  let node = do r1 <- randM 94.0 102.0
                r2 <- randM (-1.5) 1.5
                r3 <- randM 0.0 1.0
                r4 <- randM 11.0 15.0
                r5 <- randM 0.0 1.0
                r6 <- randM 12.0 15.6
                r7 <- randM 0.0 1.0
                r8 <- randM (-1.0) 1.0
                let f = r1 + lag (lfSaw ar (7 + r2) r3 * r4) 0.1
                    a = lfPulse kr (1.0 / r6) r7 0.16 * 0.05
                    b = sinOsc ar (midiCPS f) r5 * a
                return (rotate2 b (silent 1) r8)
      apf_r i = do r1 <- randM 0.0 0.06
                   r2 <- randM 0.7 2.0
                   return (allpassL i 0.07 r1 r2)
  d <- return . sum =<< sequence (replicate 6 node)
  w <- chainM 12 apf_r d
  return (d * 0.7 + w * 0.3))
