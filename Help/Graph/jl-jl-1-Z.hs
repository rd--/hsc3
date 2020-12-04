-- http://sccode.org/1-Z (jl)
let a = lag (impulse KR 8 0) 0.1
    b = crackle AR (lag (abs (lfSaw KR 3 0)) 0.1 * 1.8)
    c = a * b
    d = lag (impulse KR 2 0 + impulse KR 4 0.5) 0.1
    e = blip AR 4.9 7 * 0.4
    f = d * e
in tanh (c + gVerb f 1 1 0.5 0.5 15 1 0.7 0.5 300 * 5) * 0.5
