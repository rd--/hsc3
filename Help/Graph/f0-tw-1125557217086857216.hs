-- https://twitter.com/redFrik/status/1125557217086857216 (f0) ; id
let scl = [0, 2.94, 4.98, 7.02, 9.96] -- pythagorean
    a = lfTri
    b = (mce [-7 .. 6] + 0.7) * 2/666
    m = degreeToKey (asLocalBuf scl) ((a kr b b * a kr b 0 * 9 + 9) `modE` 32) 12 + 24
    o = varSaw ar (midiCPS m) 0 ((a kr b 0 + 1) / 2) * ampComp kr m 440 (1/3) * a kr b b * b * 9
    s = rlpf o (lag2 m ((1 / b) `modE` 1) * 3) 1
in tanh (splay (allpassN s 0.3 (0.2 - b) 3) 1 1 0 True)

-- https://twitter.com/redFrik/status/1125557217086857216 (f0) ; id
let scl = [0, 2.94, 4.98, 7.02, 9.96] -- pythagorean
    a = lfTri
    b = (mce [-7 .. 6] + 0.7) * 2/666
    m = degreeToKey (asLocalBufId 'α' scl) ((a kr b b * a kr b 0 * 9 + 9) `modE` 32) 12 + 24
    o = varSaw ar (midiCPS m) 0 ((a kr b 0 + 1) / 2) * ampComp kr m 440 (1/3) * a kr b b * b * 9
    s = rlpf o (lag2 m ((1 / b) `modE` 1) * 3) 1
in tanh (splay (allpassN s 0.3 (0.2 - b) 3) 1 1 0 True)
