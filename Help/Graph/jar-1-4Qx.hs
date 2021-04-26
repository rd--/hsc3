-- http://sccode.org/1-4Qx (jar)
let f = lfCub KR 9.1 0 * 100 + mce [200,300,400,500]
    g = lfCub KR (1 / mce [2,3,5,7]) 0 * 0.5 + lfCub KR 9 0
    h = lfCub KR (1/2) 0 * 0.4 + 0.5
    s = lfCub AR f 0 * lag (g `greater_than` h) 0.1
in splay s 1 0.075 0 True
