-- wial (rd, 2006-09-21)
let wial_m :: UId m => m UGen
    wial_m = do
      let pls c d f = do let t = pulseDivider c d 0
                             e = decay2 t 0.05 0.75
                             o = sinOsc AR (toggleFF t * f + f * 2) 0
                         n0 <- tiRandM 0 1 t
                         return (o * e * n0 * 0.5)
          smpl f = [(4,6,f,0.0075)
                   ,(2,6,f * 2,0.0175)
                   ,(1,2,f * 16,0.025)
                   ,(1,5,f * 64,0.005)
                   ,(1,4,f * 128,0.035)
                   ,(1,3,f * 256,0.15)
                   ,(2,3,f * 512,0.35)]
          plss c (d0,d1,f,a) = fmap (* a) (pls c (mce2 d0 d1) f)
          clk = impulse AR 16 0
      n0 <- dustM KR 2
      f <- tWChooseM n0 (mce2 (20 * 0.66) 20) (mce2 0.25 0.75) 0
      fmap sum (mapM (plss clk) (smpl f))
in uid_st_eval wial_m
