-- tGaussRand ; c.f. tRand
let t = dust 'α' kr 10
    f = X.tGaussRand 'β' 300 3000 t
    o = sinOsc ar f 0
    l = X.tGaussRand 'γ' (-1) 1 t
in pan2 o l 0.1
