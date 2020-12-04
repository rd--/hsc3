-- https://twitter.com/redFrik/status/456384156159574016 (f0)
let a = 1 / mce [3,12,4,1,6,2]
    s = lag3 (sinOsc AR a 0) (abs (sinOsc AR (2.67 ** a) 0)) * 99
    f = ((sinOsc AR ((1 / a) / 9) a `greater_than` 0) * 20 + 99) / a
in splay (sinOsc AR (hpf (ringz s f 1) 440) 0) 1 1 0 True * 0.25
