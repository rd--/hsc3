-- cymbalism accelerando (jmcc) #2 ; texture=overlap,4,4,4,inf
let enumFromN e i = let j = fromEnum e in [j .. j + i]
    y n = let f1 = rand n 500 2500
              f2 = rand n 0 8000
              f = map (\e -> rand e f1 (f1 + f2)) (enumFromN n 15)
              rt = map (\e -> rand e 1 5) (enumFromN n 15)
        in klankSpec f (replicate 15 1) rt
    z = mce2 (y 'α') (y 'β')
    w = whiteNoise 'γ' AR * 0.02
    tf = xLine KR (linRand 'δ' 0.5 4.5 0) (rand 'ε' 0.5 35.5) 12 DoNothing
    t = impulse AR tf 0
    s = decay t 0.004 * w
in klank s 1 0 1 (mceTranspose z)
