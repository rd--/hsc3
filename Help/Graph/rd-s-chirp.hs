-- s-chirp (rd, 2006-10-08)
let x = mouseX kr 15 0 Linear 0.1
    y = mouseY kr 15 27 Linear 0.1
    t = dust 'α' kr 9
    b = tChoose 'β' t (mce [36,48,60,72])
    n = lfNoise1 'γ' kr (mce2 3 3.05) * 0.04
    d = tiRand 'δ' x y t
    e = decay2 t 0.005 (tRand 'ε' 0.02 0.15 t)
    k = degreeToKey (asLocalBuf 'a' [0,2,3.2,5,7,9,10]) d 12
    f = midiCPS (b + k + n)
    m = e * sinOsc ar f 0 * 0.2
    u = pulseDivider t 9 0
    r0 = tRand 'ζ' 0.0075 0.125 u
    r1 = tRand 'η' 0.05 0.15 u
in m * 0.5 + allpassC m 0.15 r0 r1
