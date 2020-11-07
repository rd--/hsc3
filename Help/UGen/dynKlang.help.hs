-- dynKlang ; fixed
let s = klangSpec [800,1000,1200] [0.3,0.3,0.3] [pi,pi,pi]
in dynKlang AR 1 0 s * 0.4

-- dynKlang ; fixed randomised
let f = map (\z -> rand z 600 1000) ['a'..'l']
    s = klangSpec f (replicate 12 1) (replicate 12 0)
in dynKlang AR 1 0 s * 0.05

-- dynKlang ; dynamic frequency modulation
let f = mce3 800 1000 1200 + sinOsc KR (mce3 2 3 4.2) 0 * mce3 13 24 12
    a = mce3 0.3 0.3 0.3
    p = mce3 pi pi pi
in dynKlang AR 1 0 (klangSpec_mce f a p) * 0.1

-- dynKlang ; https://www.listarc.bham.ac.uk/lists/sc-users/msg66911.html
let k = 16 :: Int
    f i = (((fromIntegral i ** range_hs (0.3,0.7) (lag (lfNoise0 i KR 1) 0.1)) + 1) * 99
          ,max 0 (lfNoise1 i KR (linRand i 0 10 0)))
    (frq,amp) = unzip (map f [0 .. k - 1])
    s = dynKlang AR 1 0 (klangSpec frq amp (replicate k 0)) * 0.01
in pan2 s 0 1
