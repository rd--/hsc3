-- saucer base (jmcc) #6 ; texture=overlap,2,6,4,inf
let a = rand 'α' 0 20
    b = rand 'β' 0 1000
    c = rand 'γ' 0 5000
    p = rand 'δ' (-1) 1
    o = sinOsc AR a 0 * b + (1.1 * b)
    o' = sinOsc AR o 0 * c + (1.1 * c)
in pan2 (sinOsc AR o' 0 * 0.1) p 1
