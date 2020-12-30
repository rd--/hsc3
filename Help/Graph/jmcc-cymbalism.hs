-- cymbalism (jmcc) #2 ; texture=overlap,3,6,6,inf
uid_st_eval (do
  let p = replicate 15
  f1 <- randM 500 2500
  f2 <- randM 0 8000
  let y = do f <- sequence (p (randM f1 (f1 + f2)))
             rt <- sequence (p (randM 1 5))
             return (klankSpec f (p 1) rt)
  z <- clone 2 y
  n <- fmap (* 0.03) (whiteNoiseM AR)
  tf <- randM 0.5 3.5
  let t = impulse AR tf 0
      s = decay t 0.004 * n
  return (klank s 1 0 1 (mceTranspose z)))

-- cymbalism (jmcc) #2 ; event control
let f c (g,x,y,z,_,_,_,_) =
      let enumFromN e i = let j = fromEnum e in [j .. j + i]
          mk_spc n =
            let f1 = x * 2000 + 500
                f2 = rand (c,n) 0 8000
                f3 = map (\e -> tRand (c,e) f1 (f1 + f2) g) (enumFromN n 15)
                rt = map (\e -> tRand (c,e) 1 5 g) (enumFromN n 15)
            in klankSpec f3 (replicate 15 1) rt
          spc = mce2 (mk_spc 'α') (mk_spc 'β')
          whn = whiteNoise (c,'γ') AR * z * 0.1
          sig = decay (trig g controlDur) (y * 0.01) * whn
      in dynKlank sig 1 0 1 (mceTranspose spc)
in mix (rEventVoicer 16 f) * control KR "gain" 1
