-- bowed string (jmcc) ; texture=overlap,5,2,12,inf
let root = 5
    scale = map (+ root) [0,2,4,5,7,9,11]
    oct = [24,36,48,60,72,84]
    f = midiCps (lchoose scale + lchoose oct)
    x = mce2 (brownNoise ar) (brownNoise ar) * 0.007 * max 0 (lfNoise1 kr (expRand 0.125 0.5) * 0.6 + 0.4)
    geom n i z = mce (take n (iterate (* z) i))
    iota n i z = mce (take n (iterate (+ z) i))
    d = klankSpec_mce (iota 12 f f) (geom 12 1 (rand 0.7 0.9)) (X.rRandN 12 1.0 3.0)
    k = klank x 1 0 1 d
in softClip (k * 0.1)

-- bowed string (jmcc) ; texture=overlap,5,2,12,inf ; monad
uid_st_eval (do
  let root = 5
      scale = map (+ root) [0,2,4,5,7,9,11]
      oct = [24,36,48,60,72,84]
  n0 <- replicateM 2 (brownNoiseM ar)
  r0 <- expRandM 0.125 0.5
  r1 <- randM 0.7 0.9
  r2 <- X.rRandNM 12 1.0 3.0
  f <- midiCps `fmap` (Control.Monad.liftM2 (+) (lchooseM scale) (lchooseM oct))
  n1 <- lfNoise1M kr r0
  let x = mce n0 * 0.007 * max 0 (n1 * 0.6 + 0.4)
      geom n i z = mce (take n (iterate (* z) i))
      iota n i z = mce (take n (iterate (+ z) i))
      d = klankSpec_mce (iota 12 f f) (geom 12 1 r1) r2
      k = klank x 1 0 1 d
  return (softClip (k * 0.1)))

-- bowed string (jmcc) ; event control
let f _ (g,x,y,z,_,_,_,p,_,_) =
      let n0 = mce2 (brownNoise ar) (brownNoise ar)
          r0 = expRand 0.125 0.5
          r1 = rand 0.7 0.9
          r2 = X.rRandN 12 1.0 3.0
          fq = midiCps p
          n1 = lfNoise1 kr r0
          exc = n0 * 0.005 * z * lagUD g (y * 0.1) (y * 4)
          geom n i m = mce (take n (iterate (* m) i))
          iota n i m = mce (take n (iterate (+ m) i))
          d = klankSpec_mce (iota 12 fq fq) (geom 12 1 r1) r2
      in softClip (dynKlank exc 1 0 1 d)
in mix (eventVoicer 16 f) * control kr "gain" 0.75

-- bowed string (jmcc) ; event control ; id
let f c (g,_,y,z,_,_,_,p,_,_) =
      let n0 = mce2 (brownNoiseId (c,'α') ar) (brownNoiseId (c,'β') ar)
          r0 = expRandId (c,'γ') 0.125 0.5
          r1 = randId (c,'δ') 0.7 0.9
          r2 = X.rRandNId 12 (c,'ε') 1.0 3.0
          fq = midiCps p
          n1 = lfNoise1Id (c,'ζ') kr r0
          exc = n0 * 0.005 * z * lagUD g (y * 0.1) (y * 4)
          geom n i m = mce (take n (iterate (* m) i))
          iota n i m = mce (take n (iterate (+ m) i))
          d = klankSpec_mce (iota 12 fq fq) (geom 12 1 r1) r2
      in softClip (dynKlank exc 1 0 1 d)
in mix (eventVoicer 16 f) * control kr "gain" 0.75
