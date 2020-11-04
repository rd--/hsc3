-- tWChoose ; composite of tWindex and select
let x = mouseX KR 1 1000 Exponential 0.1
    d = dust 'α' AR x
    a = mce [sinOsc AR 220 0
            ,saw AR 440
            ,pulse AR 110 0.1]
    w = mce [0.5, 0.35, 0.15]
    o = tWChoose 'β' d a w 0
in o * 0.1
