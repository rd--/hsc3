-- http://sccode.org/1-4Qx (jar)
let l = lfCub
    f = l KR 9.1 0 * 100 + mce [200,300..500]
    g = l KR 9 0 + (l KR (1 / mce [2,3,5,7]) 0 * 0.5)
    h = l KR (1/2) 0 * 0.4 + 0.5
in splay (l AR f 0 * (lag (g `greater_than` h) 0.1 / 2)) 1 1 0 True * 0.15
