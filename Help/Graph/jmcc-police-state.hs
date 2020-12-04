-- police state (jmcc) #2
uid_st_eval (do
  let nd = do r0 <- randM 0.02 0.12
              r1 <- randM 0 (pi * 2)
              r2 <- randM 0 600
              r3 <- randM 700 1300
              r4 <- randM (-1) 1
              r5 <- randM 80 120
              n0 <- lfNoise2M AR r5
              let f = sinOsc KR r0 r1 * r2 + r3
              return (pan2 (sinOsc AR f 0 * n0 * 0.1) r4 1)
  ns <- clone 4 nd
  n0 <- clone 2 (lfNoise2M KR 0.4)
  n1 <- lfNoise2M AR (n0 * 90 + 620)
  n2 <- lfNoise2M KR (mce2 0.3 0.301)
  let e = n1 * (n2 * 0.15 + 0.18)
  return (combL (mix ns + e) 0.3 0.3 3))
