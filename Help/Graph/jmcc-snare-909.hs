-- snare-909 (jmcc)
let x = mouseX KR 1 4 Linear 0.2
    y = mouseY KR 0.25 0.75 Exponential 0.2
    tr = impulse KR (3 * x) 0
    n = whiteNoise 'α' AR
    v = tRand 'β' 0.25 1.0 tr
    e a b = envGen AR tr 1 0 1 DoNothing (envPerc a b)
    e1 = e 0.0005 0.055
    e2 = e 0.0005 0.075
    e3 = e 0.0005 0.4
    e4 = e 0.0005 0.283
    t1 = lfTri AR 330 0
    t2 = lfTri AR 185 0
    x1 = lpf n 7040 * 0.1 + v
    x2 = hpf x1 523
    m1 = t1 * e1 * 0.25 + t2 * e2 * 0.25
    m2 = x1 * e3 * 0.20 + x2 * e4 * 0.20
in pan2 (m1 + m2) 0 y
