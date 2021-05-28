-- f0 ; https://twitter.com/redFrik/status/1105496695637454848
let b = mce [1 .. 15] / 151
    w = ceil (varSaw KR b b (1 / 5.15) + 1.5)
    s = select ((ceil (varSaw KR (1 / 15) (1 / 5) b * 5) + 5) / 5) (mce [51 * 1.5, 51, 151])
    x = lag s b
    y = varSaw KR (5 + b) 0 0.5
    z = varSaw KR b b b * (b / 5) + mce [1.5, 5, 1]
    m = varSaw KR (5 - b) b b * 5 + 5
    o = varSaw AR ((w * x + y) * z) b ((varSaw KR b 0 0.5 + 5) / 15) * (m `greater_than` 1.515)
    f = 1515 ** (varSaw KR ((1 - b) / 5) 0 0.5 / 15 + 1 + b)
    rq = 1.5 ** varSaw KR b 0 0.5 / 5
in splay (bLowPass o f rq) 1 1 0 True / 5
