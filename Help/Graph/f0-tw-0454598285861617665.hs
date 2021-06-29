-- https://twitter.com/redFrik/status/454598285861617665 (f0)
let b = mce [9,8 .. 1]
    c = lfTri ar (3 ** lfTri ar (1 / b) (b / 9)) 0
    d = lfTri ar (1 / b) 0 `modE` 1 / 9 + 0.01
    f = 2 ** roundE (lfTri ar (b / 99) 0) * 99 * b
    o = grainSin 2 c d f 0 (-1) 512
in splay (tanh o) 1 1 0 True / 2
