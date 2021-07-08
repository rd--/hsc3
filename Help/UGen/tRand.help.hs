-- tRand
let t = dustId 'α' kr (mce2 5 12)
    f = tRandId 'β' (mce2 200 1600) (mce2 500 3000) t
in sinOsc ar f 0 * 0.1

-- tRand ; monadic variant
uid_st_eval (
  do t <- dustM kr (mce2 5 12)
     f <- tRandM (mce2 200 1600) (mce2 500 3000) t
     return (sinOsc ar f 0 * 0.1))

-- tRand ; c.f. X.tGaussRand
let t = dustId 'α' kr 10
    f = tRandId 'β' 300 3000 t
    o = sinOsc ar f 0
    l = tRandId 'γ' (-1) 1 t
in pan2 o l 0.1

-- tRand ; equal (sanity)
let t = dustId 'α' kr 1
    f = tRandId 'β' 120 240 t
in sinOsc ar (mce2 f f) 0 * 0.1
