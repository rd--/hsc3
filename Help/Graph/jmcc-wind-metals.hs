-- wind metals (jmcc) ; texture=overlap,5,2,12,inf
let n = 6
    base = expRand 60 4000
    exc = mceFill 2 (\_ -> brownNoise ar) * 0.007 * max 0 (lfNoise1 kr (expRand 0.125 0.5) * 0.75 + 0.25)
    k = klankSpec_mce (mceFill n (\_ -> rand base (base + rand 500 8000))) (mceConst n 1) (mceFill n (\_ -> rand 0.1 2))
in softClip (klank exc 1 0 1 k * 0.1)

-- wind metals (jmcc) ; texture=overlap,5,2,12,inf ; monad
uid_st_eval (do
  let n = 6
  base <- expRandM 60 4000
  rng <- randM 500 8000
  n0 <- replicateM 2 (brownNoiseM ar)
  r0 <- expRandM 0.125 0.5
  n1 <- lfNoise1M kr r0
  f <- sequence (replicate n (randM base (base + rng)))
  dt <- sequence (replicate n (randM 0.1 2))
  let exc = mce n0 * 0.007 * max 0 (n1 * 0.75 + 0.25)
      k = klankSpec f (replicate n 1) dt
      s = klank exc 1 0 1 k
  return (softClip (s * 0.1)))

-- wind metals (jmcc) ; texture=overlap,5,2,12,inf ; id
let n = 6
    base = expRandId 'α' 60 4000
    rng = randId 'β' 500 8000
    n0 = mce2 (brownNoiseId 'γ' ar) (brownNoiseId 'δ' ar)
    r0 = expRandId 'ε' 0.125 0.5
    n1 = lfNoise1Id 'ζ' kr r0
    f = X.randNId n 'η' base (base + rng)
    dt = X.randNId n 'θ' 0.1 2
    exc = n0 * 0.007 * max 0 (n1 * 0.75 + 0.25)
    k = klankSpec_mce f (mce (replicate n 1)) dt
    s = klank exc 1 0 1 k
in softClip (s * 0.1)

-- wind metals (jmcc) ; event-control
let f (c,g,x,y,z,_,_,_,_,_,_) =
      let n = 6
          base = linExp x 0 1 60 4000
          rng = tRand 500 8000 g
          n0 = mce2 (brownNoise ar) (brownNoise ar)
          r0 = tExpRand 0.125 0.5 g
          n1 = lfNoise1 kr r0
          fq = X.tRandN n base (base + rng) g
          dt = X.tRandN n 0.1 2 g
          exc = n0 * 0.01 * z * lagUD g y 2
          k = klankSpec_mce fq (mce (replicate n 1)) dt
          s = dynKlank exc 1 0 1 k
      in softClip (dynKlank exc 1 0 1 k)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- wind metals (jmcc) ; event-control ; id
let f (c,g,x,y,z,_,_,_,_,_,_) =
      let n = 6
          base = linExp x 0 1 60 4000
          rng = tRandId (c,'β') 500 8000 g
          n0 = mce2 (brownNoiseId (c,'γ') ar) (brownNoiseId (c,'δ') ar)
          r0 = tExpRandId 'ε' 0.125 0.5 g
          n1 = lfNoise1Id 'ζ' kr r0
          fq = X.tRandNId n 'η' base (base + rng) g
          dt = X.tRandNId n 'θ' 0.1 2 g
          exc = n0 * 0.01 * z * lagUD g y 2
          k = klankSpec_mce fq (mce (replicate n 1)) dt
          s = dynKlank exc 1 0 1 k
      in softClip (dynKlank exc 1 0 1 k)
in mix (eventVoicer 16 f) * control kr "gain" 1
