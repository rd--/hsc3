-- tExpRand
let f = tExpRand 'α' 300.0 3000.0 (dust 'β' kr 10)
in sinOsc ar f 0 * 0.1

-- tExpRand ; monadic form
uid_st_eval
  (do f <- tExpRandM 300.0 3000.0 =<< dustM kr 10
      return (sinOsc ar f 0 * 0.1))
