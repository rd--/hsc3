-- http://sccode.org/1-4Qy (f0)
let a = sinOsc
    z = a KR (1 / mce2 8 7) 0 * a KR (1 / 30) 0 * 9
    l = midiCPS (mce [56,62 .. 98])
    m = a AR (1 / mce2 4 3) 0
    o = a AR (select z l) 0 * m
in tanh (combN o 1 (1 / mce2 6 5) 9) * 0.1
