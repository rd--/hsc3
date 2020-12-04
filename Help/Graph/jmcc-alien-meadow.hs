-- alien meadow (jmcc) #6 ; texture=overlap,2,6,6,inf
let b = rand 'α' 0 5000
    f = sinOsc AR (rand 'β' 0 20) 0 * b * 0.1 + b
in pan2 (sinOsc AR f 0) (rand 'γ' (-1) 1) (sinOsc AR (rand 'δ' 0 20) 0 * 0.05 + 0.05)
