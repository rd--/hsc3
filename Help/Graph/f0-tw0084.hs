-- http://sccode.org/1-4Qy (f0)
let a = saw ar
    f = a (mce [5,7..15] * 19) * 99 + 199
    g = a (mce [1,3..13] * 29) * 199 + 299
    w = a (mce [3,5..11] * (a 3 * 2 + 3)) * 299 + 399
in splay (formant ar f g w) 1 1 0 True / 9
