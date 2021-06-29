-- http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)
let a i j k l = sinOsc ar i j * k + l
    f = a (a 0.11 0 1 0) 0 1 0
    p_f = a (95 * a 0.01 0 1 1) 0 (a 0.005 0 50 0) 100
    p = a p_f (a (mce2 98 97) 0 1 0) (pi + a 0.0005 0 1 0) 0
in tanh (a f p 1 0) * 0.1
