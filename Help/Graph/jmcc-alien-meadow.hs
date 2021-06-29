-- alien meadow (jmcc) #6 ; texture=overlap,2,6,6,inf
let b = rand 0 5000
    f = sinOsc ar (rand 0 20) 0 * b * 0.1 + b
in pan2 (sinOsc ar f 0) (rand2 1) (sinOsc ar (rand 0 20) 0 * 0.05 + 0.05)

-- alien meadow (jmcc) #6 ; texture=overlap,2,6,6,inf ; id
let b = randId 'α' 0 5000
    f = sinOsc ar (randId 'β' 0 20) 0 * b * 0.1 + b
in pan2 (sinOsc ar f 0) (randId 'γ' (-1) 1) (sinOsc ar (randId 'δ' 0 20) 0 * 0.05 + 0.05)
