-- babbling brook (jmcc) #Sc3
let b f m a g _ = let n3 = lpf (brownNoise ar) f * m + a
                      n4 = onePole (brownNoise ar) 0.99
                  in rhpf n4 n3 0.03 * g
    x = mceFill 2 (b 14 400 500 0.006)
    y = mceFill 2 (b 20 800 1000 0.010)
in x + y

-- babbling brook (jmcc) #Sc3
uid_st_eval (do
  let b f m a g = do n1 <- brownNoiseM ar
                     n2 <- brownNoiseM ar
                     let n3 = lpf n2 f * m + a
                         n4 = onePole n1 0.99
                     return (rhpf n4 n3 0.03 * g)
  x <- replicateM 2 (b 14 400 500 0.006)
  y <- replicateM 2 (b 20 800 1000 0.010)
  return (mce x + mce y))
