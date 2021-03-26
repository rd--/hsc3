-- https://twitter.com/redFrik/status/1138498427241861122 (f0)
let f = sqrt 9
    b = mce2 f 9.999
    e = pitchShift
    a = sinOscFB
    d = max (a AR (b/99) 0) 0
    t1 = ugen_if (a AR (a AR (9/999) 0) 0 `greater_than` a AR (9/99) 0) f (9/b)
    t2 = ugen_if (a AR (9/99) 0 `less_than` a AR (99/9999) 0) (b/9) f
    f1 = lag (9.9 * b * t1 * t2) 0.1
    s1 = a AR f1 d * a AR (b/9) 0 * d
    c = e s1 (9 / b) (9 / sqrt b) (b / 999) (b / 99)
    s2 = gVerb (c * d * d * d) 99 9 (9/999) 0.5 15 1 0.7 0.5 300
    s3 = s2 / 9 + e c (f/9) (f/9) 0 0
in mix (mceTranspose (hpf (splay s3 1 1 0 True) 9 / 9))
