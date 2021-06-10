-- https://www.listarc.bham.ac.uk/lists/sc-users/msg18379.html (f0)
let t = t2a 0 0
    o0 = lfSaw AR 3 0 * 9 + 99
    s0 = sweep t (mce [3..9]) `modE` o0
    s1 = sweep t (mce [33,32..3]) `modE` 128 .&. s0
    o1 = sinOsc AR (midiCPS (s1 + 33)) 0 * pi
in splay (sinOsc AR 9 o1) 1 1 0 True / 12
