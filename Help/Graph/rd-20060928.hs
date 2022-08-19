-- f-lets (rd. 2006-09-28)
let f_let t g j n f =
      let pd = pulseDivider t j 0
      in formlet pd (f * tiRand (mce2 2 1) n pd) (tRand 0.01 0.04 pd) (tRand 0.05 0.10 pd) * g
    mk_n t =
      sum [f_let t 0.15 2 9 (mce2 200 400)
          ,f_let t 0.25 2 9 (mce2 (200 + tRand 0 1 t) (400 + tRand 0 1 t))
          ,f_let t 0.05 4 5 (mce2 25 50)
          ,f_let t 0.15 4 5 (mce2 (25 + tRand 0 1 t) (50 + tRand 0 1 t))
          ,f_let t 0.5 1 16 (mce2 300 600) * latch (coinGate 0.2 t) t]
in mk_n (impulse ar 24 0) * (lfNoise0 kr 2 * 0.25 + 0.25)

-- f-lets (rd. 2006-09-28) ; monad
uid_st_eval (do
  let f_let t g j n f = do
           let pd = pulseDivider t j 0
           r0 <- tiRandM (mce2 2 1) n pd
           r1 <- tRandM 0.01 0.04 pd
           r2 <- tRandM 0.05 0.10 pd
           return (formlet pd (f * r0) r1 r2 * g)
      mk_n t = do
           r0 <- tRandM 0 1 t
           r1 <- tRandM 0 1 t
           r2 <- tRandM 0 1 t
           r3 <- tRandM 0 1 t
           r4 <- coinGateM 0.2 t
           sequence [f_let t 0.15 2 9 (mce2 200 400)
                    ,f_let t 0.25 2 9 (mce2 (200 + r0) (400 + r1))
                    ,f_let t 0.05 4 5 (mce2 25 50)
                    ,f_let t 0.15 4 5 (mce2 (25 + r2) (50 + r3))
                    ,let lr = fmap (* latch r4 t)
                     in lr (f_let t 0.5 1 16 (mce2 300 600))]
      tr = impulse ar 24 0
  n <- lfNoise0M kr 2
  return . (* (n * 0.25 + 0.25)) . sum =<< mk_n tr)
