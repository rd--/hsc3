-- http://sccode.org/1-Z (jl)
let a = lag (impulse kr 8 0) 0.1
    b = crackle ar (lag (abs (lfSaw kr 3 0)) 0.1 * 1.8)
    c = a * b
    d = lag (impulse kr 2 0 + impulse kr 4 0.5) 0.1
    e = blip ar 4.9 7 * 0.4
    f = d * e
in tanh (c + gVerb f 1 1 0.5 0.5 15 1 0.7 0.5 300 * 5) * 0.5
