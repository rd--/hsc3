-- http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)
let f = roundE (lfPar AR (1/14) 0) * 20 + 80
    a = pulse AR (mce [1..4]) 0.35
    n = mce (map (flip brownNoise AR) ['α','β','γ','δ']) * a
    z i = mce2 ((i + 1) * f) ((i * f) + (i + 1 / 3))
    o = lfPar AR (mce (map z [0..3])) 0
    (s1,s2) = unmce2 (splay ((o `greater_than` n) / 3) 1 1 0 True)
in (s1 + s2) * 0.1
