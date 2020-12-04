-- https://twitter.com/redFrik/status/1125557217086857216 (f0)
let scl = [0, 2.94, 4.98, 7.02, 9.96] -- pythagorean
    a = lfTri
    b = (mce [-7 .. 6] + 0.7) * 2/666
    m = degreeToKey (asLocalBuf 'α' scl) ((a KR b b * a KR b 0 * 9 + 9) `modE` 32) 12 + 24
    o = varSaw AR (midiCPS m) 0 ((a KR b 0 + 1) / 2) * ampComp KR m 440 (1/3) * a KR b b * b * 9
    s = rlpf o (lag2 m ((1 / b) `modE` 1) * 3) 1
in tanh (splay (allpassN s 0.3 (0.2 - b) 3) 1 1 0 True)
