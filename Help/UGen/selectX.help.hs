-- selectX ; composite ugen graph
let n = 3/2
    f = mce2 440 441
    a = mce [sinOsc ar f 0, saw ar f, pulse ar f 0.1]
in mix (selectX (lfSaw kr 1 0 * n + n) a * 0.1)

-- selectX ; as sequencer
let n = 10
    a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
    c = n / 2
    f = selectX (lfSaw kr 0.5 0 * c + c) a
in saw ar f * 0.1
