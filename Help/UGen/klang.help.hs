-- klang ; note SC2 had mul/add inputs
let f = [440,550..1100]
    a = take 7 (cycle [0.05, 0.02])
    p = replicate 7 0
in klang ar 1 0 (klangSpec f a p)

-- klang
let s = klangSpec [800,1000,1200] [0.3,0.3,0.3] [pi,pi,pi]
in klang ar 1 0 s * 0.4

-- klang
let s = klangSpec [800,1000,1200] [1,1,1] [0,0,0]
in klang ar 1 0 s * 0.1

-- klang
let f = map (\z -> rand z 600 1000) ['a'..'l']
    s = klangSpec f (replicate 12 1) (replicate 12 0)
in klang ar 1 0 s * 0.02
