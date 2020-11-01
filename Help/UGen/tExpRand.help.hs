-- tExpRand
let f = tExpRand 'α' 300.0 3000.0 (dust 'β' KR 10)
in sinOsc AR f 0 * 0.1

-- tExpRand ; monadic form
uid_st_eval
  (do f <- tExpRandM 300.0 3000.0 =<< dustM KR 10
      return (sinOsc AR f 0 * 0.1))
