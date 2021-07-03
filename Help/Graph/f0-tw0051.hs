-- http://sccode.org/1-4Qy (f0)
let ascii_u = mce . map (constant . fromEnum)
    i = a (ascii_u "sunday")
    f = a (9 / ascii_u "slow") * 400 + 500
    w = a (7 / ascii_u "coding") + 1.1
    a = saw ar
    l = splay (bBandPass i f w / 5) 1 1 0 True
in gVerb l 10 3 0.5 0.5 15 1 0.7 0.5 300 * 0.2
