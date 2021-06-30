-- cymbalism (jmcc) #2 ; texture=overlap,3,6,6,inf
let p = 15
    spec _ = let f1 = rand 500 2500
                 f = mceFill p (\_ -> rand f1 (f1 + rand 0 8000))
                 rt = mceFill p (\_ -> rand 1 5)
             in klankSpec_mce f (mceFill p (const 1)) rt
    t = impulse ar (rand 0.5 3.5) 0
    s = whiteNoise ar * decay t 0.004 * 0.03
in mceFill 2 (\n -> klank s 1 0 1 (spec n))

-- cymbalism (jmcc) #2 ; texture=overlap,3,6,6,inf ; monad
uid_st_eval (do
  let p = replicate 15
  f1 <- randM 500 2500
  f2 <- randM 0 8000
  let y = do f <- sequence (p (randM f1 (f1 + f2)))
             rt <- sequence (p (randM 1 5))
             return (klankSpec f (p 1) rt)
  z <- replicateM 2 y
  n <- fmap (* 0.03) (whiteNoiseM ar)
  tf <- randM 0.5 3.5
  let t = impulse ar tf 0
      s = decay t 0.004 * n
  return (klank s 1 0 1 (mceTranspose (mce z))))

-- cymbalism (jmcc) #2 ; event control
let f _ (g,x,y,z,_,_,_,_,_,_) =
      let mk_spc _ =
            let f1 = x * 2000 + 500
                f3 = mceFill 15 (\_ -> tRand f1 (f1 + rand 0 8000) g)
                rt = mceFill 15 (\_ -> tRand 1 5 g)
            in klankSpec_mce f3 (mceFill 15 (const 1)) rt
          spc = mceFill 2 mk_spc
          whn = whiteNoise ar * z * 0.1
          sig = decay (trig g controlDur) (y * 0.01) * whn
      in dynKlank sig 1 0 1 (mceTranspose spc)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- cymbalism (jmcc) #2 ; event control ; id
let f c (g,x,y,z,_,_,_,_,_,_) =
      let enumFromN e i = let j = fromEnum e in [j .. j + i]
          mk_spc n =
            let f1 = x * 2000 + 500
                f2 = rand (c,n) 0 8000
                f3 = map (\e -> tRand (c,e) f1 (f1 + f2) g) (enumFromN n 15)
                rt = map (\e -> tRand (c,e) 1 5 g) (enumFromN n 15)
            in klankSpec f3 (replicate 15 1) rt
          spc = mce2 (mk_spc 'α') (mk_spc 'β')
          whn = whiteNoise (c,'γ') ar * z * 0.1
          sig = decay (trig g controlDur) (y * 0.01) * whn
      in dynKlank sig 1 0 1 (mceTranspose spc)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- cymbalism (jmcc) #2 ; event control ; id
let f c (g,x,y,z,_,_,_,_,_,_) =
      let enumFromN e i = let j = fromEnum e in [j .. j + i]
          mk_spc n =
            let f1 = x * 2000 + 500
                f2 = randId (c,n) 0 8000
                f3 = map (\e -> tRandId (c,e) f1 (f1 + f2) g) (enumFromN n 15)
                rt = map (\e -> tRandId (c,e) 1 5 g) (enumFromN n 15)
            in klankSpec f3 (replicate 15 1) rt
          spc = mce2 (mk_spc 'α') (mk_spc 'β')
          whn = whiteNoiseId (c,'γ') ar * z * 0.1
          sig = decay (trig g controlDur) (y * 0.01) * whn
      in dynKlank sig 1 0 1 (mceTranspose spc)
in mix (eventVoicer 16 f) * control kr "gain" 1
