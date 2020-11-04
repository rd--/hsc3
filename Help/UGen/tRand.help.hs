-- tRand
let t = dust 'α' KR (mce2 5 12)
    f = tRand 'β' (mce2 200 1600) (mce2 500 3000) t
in sinOsc AR f 0 * 0.1

-- tRand ; monadic variant
uid_st_eval (
  do t <- dustM KR (mce2 5 12)
     f <- tRandM (mce2 200 1600) (mce2 500 3000) t
     return (sinOsc AR f 0 * 0.1))
