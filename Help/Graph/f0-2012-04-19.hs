-- https://www.listarc.bham.ac.uk/lists/sc-users/msg17536.html (f0)
let s0 = lfSaw AR 10 0 * 0.01
    t0 = lfTri AR (mce2 5 6 * 0.1) 0
    t1 = lfTri KR 0.1 0 * 0.05 + 0.05
    s1 = limiter (brf s0 t0 1) 1 0.01
    o = combN s1 0.1 (roundTo t1 0.01) 1
in o * 0.1
