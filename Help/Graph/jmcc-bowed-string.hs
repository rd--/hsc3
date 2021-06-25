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

-- bowed string (jmcc) ; event control
let f c (g,x,y,z,_,_,_,p,_,_) =
      let n0 = mce2 (brownNoise (c,'α') AR) (brownNoise (c,'β') AR)
          r0 = expRand (c,'γ') 0.125 0.5
          r1 = rand (c,'δ') 0.7 0.9
          r2 = X.rRandN 12 (c,'ε') 1.0 3.0
          fq = midiCPS p
          n1 = lfNoise1 (c,'ζ') KR r0
          exc = n0 * 0.005 * z * lagUD g (y * 0.1) (y * 4)
          geom n i z = mce (take n (iterate (* z) i))
          iota n i z = mce (take n (iterate (+ z) i))
          d = klankSpec_mce (iota 12 fq fq) (geom 12 1 r1) r2
      in softClip (dynKlank exc 1 0 1 d)
in mix (eventVoicer 16 f) * control KR "gain" 0.75
