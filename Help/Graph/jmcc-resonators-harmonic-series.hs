-- resonators harmonic series (jmcc) #2 ; texture=xfade,1,7,inf
let series n i j = case n of {0 -> []; _ -> i : series (n - 1) (i + j) j}
    enumFromN e i = let j = fromEnum e in [j .. j + i]
    p = 12
    noise = brownNoise 'α' AR * 0.001
    rat = [1.0,1.125,1.25,1.333,1.5,1.667,1.875,2.0,2.25,2.5,2.667,3.0,3.333,3.75,4.0]
    freq = lchoose 'β' rat * 120
    resFreqs = zipWith (+)
               (series p freq freq)
               (map (\z -> rand2 ('γ',z) 0.5) (enumFromN 'δ' p))
    spec = klankSpec
           resFreqs
           (map (\i -> 1 / (constant i + 1)) [0 .. p - 1])
           (map (\z -> rand ('ε',z) 0.5 4.5) (enumFromN 'ζ' p))
in Protect.uclone_all 'η' 2 (klank noise 1 0 1 spec)
