-- shifting pulses (rd, 2006-09-18)
let prt a f =
      let f' = f * linLin_b (sinOsc kr (rand 0.2 0.9) 0) 1 1.01 * rand 0.95 1.05
      in sinOsc ar (mce2 f f') 0 * a * mceFill 2 (\_ -> rand 0.95 1.05)
    prts n f a = sum (map (prt a) [f,f + f .. f * n])
    fmt = formant ar (mce2 20 21) (linLin_b (lfNoise2 kr 2) 10 100) 200 * 0.35
    pulses =
      let t = dust kr 0.75
          warp i = linLin i (-1) 1
          p = pulse ar (warp (mceFill 2 (\_ -> brownNoise kr)) 2 (mce2 11 15)) 0.01 * 0.1
          f = warp (mceFill 2 (\_ -> brownNoise kr)) 90 300
          rq = warp (mceFill 2 (\_ -> brownNoise kr)) 2 9
      in mrg2 (latch t t * rlpf p f rq) (sendTrig t 0 t)
in prts 2 900 0.008 + prts 9 40 0.022 + fmt + pulses

-- shifting pulses (rd, 2006-09-18) ; monad
uid_st_eval (do
  let prt a f = do
        r0 <- randM 0.2 0.9
        r1 <- randM 0.95 1.05
        r2 <- replicateM 2 (randM 0.95 1.05)
        let f' = f * linLin_b (sinOsc kr r0 0) 1 1.01 * r1
            a' = a * mce r2
            o = sinOsc ar (mce2 f f') 0
        return (o * a')
  let prts n f a = fmap sum (mapM (prt a) [f,f + f .. f * n])
  let fmt = do
        n <- lfNoise2M kr 2
        return (formant ar (mce2 20 21) (linLin_b n 10 100) 200 * 0.35)
  let pulses = do
        n0 <- replicateM 2 (brownNoiseM kr)
        n1 <- replicateM 2 (brownNoiseM kr)
        n2 <- replicateM 2 (brownNoiseM kr)
        t <- dustM kr 0.75
        let warp i = linLin i (-1) 1
            l = latch t t
            p = pulse ar (warp (mce n0) 2 (mce2 11 15)) 0.01 * 0.1
            f = warp (mce n1) 90 300
            rq = warp (mce n2) 2 9
        return (mrg2 (l * rlpf p f rq) (sendTrig t 0 t))
  p1 <- prts 2 900 0.008
  p2 <- prts 9 40 0.022
  f <- fmt
  s <- pulses
  return (p1 + p2 + f + s))
