-- f0 <https://twitter.com/redFrik/status/1338987643057688579>
let b = mce [8..18]
    x = varSaw ar (mce [18.1,81.8,1.81] + b) 1 (varSaw ar (b/81) 0 0.5)
    t = varSaw ar (8/1.818) (mce [1,8/18]) 0.5
    d = (varSaw ar (b/181.8) 0 0.5 `in_exprange` (1.81/818,1.8/181.8)) `roundTo` (1.81/818)
    c = varSaw ar (1/8) (1/b) 0.5 + 8.18 * 1.8
    z = varSaw ar (1/81.8) 0 0.5 `in_range` (1.8 ** (-1),8.1/8.18)
    p = (pluck x t (1.8/181.8) d c z * (varSaw ar (1.8/b) 0 0.5) / 1.8)
in hpf (splay p 1 1 0 True) (18/1.81)
