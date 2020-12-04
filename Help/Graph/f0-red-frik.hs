-- red_frik (f0)
let red tr n = do
      r1 <- tRandM 0.3 3 tr
      r2 <- tRandM 0.3 5 tr
      r3 <- tRandM 0 0.5 tr
      r4 <- tRandM 0.49 0.56 tr
      r5 <- tRandM 0.3 0.6 tr
      r6 <- tRandM 0.3 0.5 tr
      let o1 = fSinOsc KR r2 0 * r3 + r4
          o2 = fSinOsc KR o1 0 * r5 + r6
      return (rhpf n r1 o2 * 0.1)
    red_frik_m = do
      n <- clone 2 (brownNoiseM AR)
      let tr = impulse KR 0.1 0
      red tr n
in uid_st_eval red_frik_m * 0.1
