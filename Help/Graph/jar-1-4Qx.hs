-- http://sccode.org/1-4Qx (jar)
let f = lfCub kr 9.1 0 * 100 + mce [200,300,400,500]
    g = lfCub kr (1 / mce [2,3,5,7]) 0 * 0.5 + lfCub kr 9 0
    h = lfCub kr (1/2) 0 * 0.4 + 0.5
    s = lfCub ar f 0 * lag (g `greater_than` h) 0.1
in splay s 1 0.075 0 True
