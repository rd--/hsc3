-- http://sccode.org/1-4Qy (f0)
let a = sinOsc AR
    f = a (1 / mce2 5 6) 0 + mce2 798 912
    p = (a (1 / 16) 0 * 19 + 99) * a (mce2 9 8) 0
    m = a (a 6 0 * a 0.009 0) 0
    b = a f p * m
in tanh (a (mce2 201 301) b) * 0.1
