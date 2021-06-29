-- bottle (sc) ; texture=overlap,2,0,4,inf
let bottle_m = do
      freq <- randM 220 880
      wn <- whiteNoiseM ar
      pn <- pinkNoiseM ar
      let perc = envPerc 0.1 0.6
          ex = envGen kr 1 1 0 1 DoNothing perc * wn * 0.02
          flute = ringz ex freq 0.3
          r = resonz pn (5 + (freq / 2)) 0.1
          breath = envGen kr 1 1 0 1 DoNothing perc * r
          rapf i = do x <- linRandM 0.001 0.1 (-1)
                      return (i + allpassN i 0.1 x 1.0 * 0.5)
          cls i = let en = let c = EnvNum (-4) in (c,c,c)
                      l = envLinen_c 0.01 3.0 1.0 1 en
                      z = (breath + i) * envGen kr 1 1 0 1 RemoveSynth l
                  in mce2 z z
      f <- chainM 2 rapf flute
      return (cls f)
in uid_st_eval bottle_m
