-- s-chirp (rd, 2006-10-08)
let x = mouseX kr 15 0 Linear 0.1
    y = mouseY kr 15 27 Linear 0.1
    t = dust kr 9
    b = tChoose t (mce [36,48,60,72])
    n = lfNoise1 kr (mce2 3 3.05) * 0.04
    d = tiRand x y t
    e = decay2 t 0.005 (tRand 0.02 0.15 t)
    k = degreeToKey (asLocalBuf [0,2,3.2,5,7,9,10]) d 12
    f = midiCPS (b + k + n)
    m = e * sinOsc ar f 0 * 0.2
    u = pulseDivider t 9 0
    r0 = tRand 0.0075 0.125 u
    r1 = tRand 0.05 0.15 u
in m * 0.5 + allpassC m 0.15 r0 r1

-- s-chirp (rd, 2006-10-08) ; id
let x = mouseX kr 15 0 Linear 0.1
    y = mouseY kr 15 27 Linear 0.1
    t = dustId 'α' kr 9
    b = tChooseId 'β' t (mce [36,48,60,72])
    n = lfNoise1Id 'γ' kr (mce2 3 3.05) * 0.04
    d = tiRandId 'δ' x y t
    e = decay2 t 0.005 (tRandId 'ε' 0.02 0.15 t)
    k = degreeToKey (asLocalBufId 'a' [0,2,3.2,5,7,9,10]) d 12
    f = midiCPS (b + k + n)
    m = e * sinOsc ar f 0 * 0.2
    u = pulseDivider t 9 0
    r0 = tRandId 'ζ' 0.0075 0.125 u
    r1 = tRandId 'η' 0.05 0.15 u
in m * 0.5 + allpassC m 0.15 r0 r1
