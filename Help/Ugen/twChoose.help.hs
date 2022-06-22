-- tWChoose ; composite of tWindex and select
let x = mouseX kr 1 1000 Exponential 0.1
    d = dustId 'α' ar x
    a = mce [sinOsc ar 220 0
            ,saw ar 440
            ,pulse ar 110 0.1]
    w = mce [0.5, 0.35, 0.15]
    o = tWChooseId 'β' d a w 0
in o * 0.1
