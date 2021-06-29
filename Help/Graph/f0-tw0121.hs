-- http://sccode.org/1-4Qy (f0)
let a = sinOsc
    z = a kr (1 / mce2 8 7) 0 * a kr (1 / 30) 0 * 9
    l = midiCPS (mce [56,62 .. 98])
    m = a ar (1 / mce2 4 3) 0
    o = a ar (select z l) 0 * m
in tanh (combN o 1 (1 / mce2 6 5) 9) * 0.1
