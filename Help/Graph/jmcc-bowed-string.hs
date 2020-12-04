-- bowed string (jmcc) ; texture=overlap,5,2,12,inf
uid_st_eval (do
  let root = 5
      scale = map (+ root) [0,2,4,5,7,9,11]
      oct = [24,36,48,60,72,84]
  n0 <- clone 2 (brownNoiseM AR)
  r0 <- expRandM 0.125 0.5
  r1 <- randM 0.7 0.9
  r2 <- sequence (replicate 12 (randM 1.0 3.0))
  f <- midiCPS `fmap` (Control.Monad.liftM2 (+) (lchooseM scale) (lchooseM oct))
  n1 <- lfNoise1M KR r0
  let x = n0 * 0.007 * max 0 (n1 * 0.6 + 0.4)
      geom n i z = take n (iterate (* z) i)
      iota n i z = take n (iterate (+ z) i)
      d = klankSpec (iota 12 f f) (geom 12 1 r1) r2
      k = klank x 1 0 1 d
  return (softClip (k * 0.1)))
