-- http://fredrikolofsson.com/f0blog/?q=node/478 (f0)
let a n = lfPulse AR n 0 0.5
    b = mce [1..4]
    a1 = (a (b/32) + 1) / 8
    a2 = (a a1 + 1) * b
    a3 = a (b/64)
    a4 = a (a (b/8)) * 2 + b
    a5 = a (4/b)
    a6 = a a2
    a7 = a ((a6 + ((mix a3 + a5) * a4)) * 100)
    s = mix a7 / 8
in mce2 s s * 0.1
