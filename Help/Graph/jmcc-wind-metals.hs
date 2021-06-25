-- wind metals (jmcc) ; texture=overlap,5,2,12,inf
uid_st_eval (do
  let n = 6
  base <- expRandM 60 4000
  rng <- randM 500 8000
  n0 <- clone 2 (brownNoiseM AR)
  r0 <- expRandM 0.125 0.5
  n1 <- lfNoise1M KR r0
  f <- sequence (replicate n (randM base (base + rng)))
  dt <- sequence (replicate n (randM 0.1 2))
  let exc = n0 * 0.007 * max 0 (n1 * 0.75 + 0.25)
      k = klankSpec f (replicate n 1) dt
      s = klank exc 1 0 1 k
  return (softClip (s * 0.1)))

-- wind metals (jmcc) ; texture=overlap,5,2,12,inf
let n = 6
    base = expRand 'α' 60 4000
    rng = rand 'β' 500 8000
    n0 = mce2 (brownNoise 'γ' AR) (brownNoise 'δ' AR)
    r0 = expRand 'ε' 0.125 0.5
    n1 = lfNoise1 'ζ' KR r0
    f = X.rRandN n 'η' base (base + rng)
    dt = X.rRandN n 'θ' 0.1 2
    exc = n0 * 0.007 * max 0 (n1 * 0.75 + 0.25)
    k = klankSpec_mce f (mce (replicate n 1)) dt
    s = klank exc 1 0 1 k
in softClip (s * 0.1)

-- wind metals (jmcc) ; event-control
let f c (g,x,y,z,_,_,_,_,_,_) =
      let n = 6
          base = linExp x 0 1 60 4000
          rng = tRand (c,'β') 500 8000 g
          n0 = mce2 (brownNoise (c,'γ') AR) (brownNoise (c,'δ') AR)
          r0 = tExpRand 'ε' 0.125 0.5 g
          n1 = lfNoise1 'ζ' KR r0
          fq = X.rtRandN n 'η' base (base + rng) g
          dt = X.rtRandN n 'θ' 0.1 2 g
          exc = n0 * 0.01 * z * lagUD g y 2
          k = klankSpec_mce fq (mce (replicate n 1)) dt
          s = dynKlank exc 1 0 1 k
      in softClip (dynKlank exc 1 0 1 k)
in mix (eventVoicer 16 f) * control KR "gain" 1
