-- f0 <https://twitter.com/redFrik/status/1343283697840226307>
let b = mce [2..12] / 12
    f = mce [136.1, 163.3, 181.4, 204.1, 244.9, 272.1, 326.5, 362.8, 408.2, 489.8, 544.2]
    z = mceFill 11 (\z -> whiteNoise ('α',z::Int) AR) * ((lfTri AR b 0 + 1) / 2) + brownNoise 'β' AR
    t = lfTri AR (121 ** lfTri AR ((2 / 121) * b) 0) 0
    d = (lag2 ((1 / 212) `greater_than` lfTri AR (1/212.1) 0) (2 / b) / 12 * 1.2 + 1.2) / f
    p = pluck z t (2 / 121) d (1 / 2 * 12 / 1.2) ((lfTri AR (b/12) 0 + 1) / 2)
in splay (hpf p 12) 1 (1 / 2.1 / 2) 0 True

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 0
