-- ccomb (rd, 2006-10-07)
let lwr = 48
    spart t _ = let n = range lwr 72.0 (lfNoise2 kr 0.1)
                    e = decay2 t 0.01 (tRand 0.05 0.75 t)
                    x = e * whiteNoise ar
                    f = lag (midiCps n) 0.25
                in combC x (recip (midiCps lwr)) (recip f) (range 1 8 (lfNoise2 kr 0.1))
    t = dust kr (mce2 0.75 0.35)
in mixFill 12 (spart t) * 0.01

-- ccomb (rd, 2006-10-07) ; monad
uid_st_eval (do
  let lwr = 48
      flwr = midiCps lwr
      spart t = do n <- fmap (range lwr 72.0) (lfNoise2M kr 0.1)
                   e <- fmap (decay2 t 0.01) (tRandM 0.05 0.75 t)
                   x <- fmap (* e) (whiteNoiseM ar)
                   m <- lfNoise2M kr 0.1
                   let f = lag (midiCps n) 0.25
                       m' = range 1 8 m
                   return (combC x (recip flwr) (recip f) m')
  t <- dustM kr (mce2 0.75 0.35)
  return . (* 0.01) . sum =<< sequence (replicate 12 (spart t)))
