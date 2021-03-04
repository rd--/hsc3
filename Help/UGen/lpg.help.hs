-- lpg
let tempo = 8
    envdur = 0.25
    trig = impulse KR tempo 0
    squ = demand trig 0 (dseq 'α' dinf (mce [400,99,791,2000,200])) `lag` 0.01
    sig = sinOscFB AR squ 1.1 + whiteNoise 'β' AR * 0.1
    env = envGen KR trig 1 0 (envdur / tempo) DoNothing (envPerc 0.1 0.9)
    flt = let c_off = lfSaw KR 0.1 0 `in_range` (0,0.2)
              c_mul = lfNoise2 'γ' KR 0.1 `in_range` (0.4,0.95)
              vca = lfNoise2 'δ' KR 1 `in_range` (0,1)
          in X.lpg sig env c_off c_mul vca 1.1 1 0
in pan2 flt (0.5 * tRand 'ε' (-1) 1 trig) 0.25

-- lpg
let freq = lfNoise2 'α' KR 1 `in_range` (1,4)
    tr = impulse KR freq 0
    env = envGen KR tr 1 0 (1 / freq * 0.05) DoNothing (envPerc 0.1 1.5)
    o1 = sinOsc KR (lag3 env 0.1 `in_range` (100,10)) 0
    o2 = sinOsc AR ((lfNoise2 'β' KR 1 `in_exprange` (200,1000)) * o1) 0
    sig = whiteNoise 'γ' AR * 0.2 + o2
    flt = let c_mul = lfNoise2 'δ' KR 10.1 `in_range` (0.5,0.95)
          in X.lpg sig env 0 c_mul 0.091 1.35 1 1
in pan2 flt (sinOsc KR 0.1 0 * 0.25) 0.25

-- lpg
let tempo = 8
    envdur = 0.91
    tr = impulse KR tempo 0
    squ = demand tr 0 (dseq 'α' dinf (mce [200,291,420,191,320])) `lag` 0.01
    mdl = tWChoose 'β' tr (mce [0.5,1.0,1.5]) (mce [0.75, 0.125, 0.125]) 0
    sig = sinOscFB AR (squ * mdl) 1.1 + whiteNoise 'γ' AR * 0.1
    rnd = tRand 'δ' 0 1 tr
    env = envGen KR tr 1 0 (envdur / tempo) DoNothing (envPerc rnd (1 - rnd))
    flt = let c_off = lfSaw KR 0.1 0 `in_range` (0,0.2)
              c_mul = lfNoise2 'ε' KR 0.1 `in_range` (0.4,0.95)
              vca = lfNoise2 'ζ' KR 1 `in_range` (0,1)
          in X.lpg sig env c_off c_mul vca 1.1 1 0
in pan2 flt (0.5 * tRand 'η' (-1) 1 tr) 0.25
