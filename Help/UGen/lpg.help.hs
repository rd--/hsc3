-- lpg
let tempo = 8
    envdur = 0.25
    trig = impulse kr tempo 0
    squ = demand trig 0 (dseq 'α' dinf (mce [400,99,791,2000,200])) `lag` 0.01
    sig = sinOscFB ar squ 1.1 + whiteNoise 'β' ar * 0.1
    env = envGen kr trig 1 0 (envdur / tempo) DoNothing (envPerc 0.1 0.9)
    flt = let c_off = lfSaw kr 0.1 0 `in_range` (0,0.2)
              c_mul = lfNoise2 'γ' kr 0.1 `in_range` (0.4,0.95)
              vca = lfNoise2 'δ' kr 1 `in_range` (0,1)
          in X.lpg sig env c_off c_mul vca 1.1 1 0
in pan2 flt (0.5 * tRand 'ε' (-1) 1 trig) 0.25

-- lpg
let freq = lfNoise2 'α' kr 1 `in_range` (1,4)
    tr = impulse kr freq 0
    env = envGen kr tr 1 0 (1 / freq * 0.05) DoNothing (envPerc 0.1 1.5)
    o1 = sinOsc kr (lag3 env 0.1 `in_range` (100,10)) 0
    o2 = sinOsc ar ((lfNoise2 'β' kr 1 `in_exprange` (200,1000)) * o1) 0
    sig = whiteNoise 'γ' ar * 0.2 + o2
    flt = let c_mul = lfNoise2 'δ' kr 10.1 `in_range` (0.5,0.95)
          in X.lpg sig env 0 c_mul 0.091 1.35 1 1
in pan2 flt (sinOsc kr 0.1 0 * 0.25) 0.25

-- lpg
let tempo = 8
    envdur = 0.91
    tr = impulse kr tempo 0
    squ = demand tr 0 (dseq 'α' dinf (mce [200,291,420,191,320])) `lag` 0.01
    mdl = tWChoose 'β' tr (mce [0.5,1.0,1.5]) (mce [0.75, 0.125, 0.125]) 0
    sig = sinOscFB ar (squ * mdl) 1.1 + whiteNoise 'γ' ar * 0.1
    rnd = tRand 'δ' 0 1 tr
    env = envGen kr tr 1 0 (envdur / tempo) DoNothing (envPerc rnd (1 - rnd))
    flt = let c_off = lfSaw kr 0.1 0 `in_range` (0,0.2)
              c_mul = lfNoise2 'ε' kr 0.1 `in_range` (0.4,0.95)
              vca = lfNoise2 'ζ' kr 1 `in_range` (0,1)
          in X.lpg sig env c_off c_mul vca 1.1 1 0
in pan2 flt (0.5 * tRand 'η' (-1) 1 tr) 0.25
