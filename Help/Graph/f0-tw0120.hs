-- http://sccode.org/1-4Qy (f0)
let a = lfTri
    z = a kr (1 / mce2 7 8) 0 * a kr (1 / 9) 0 * 99
    l = midiCPS (mce [60 .. 79])
    f = select z l
    w = a kr (1 / mce2 3 4) 0 `modE` 1
    o = varSaw ar f 0 w
in combN o 1 (1 / mce2 5 6) 8 / 8
