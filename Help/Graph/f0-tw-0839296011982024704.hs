-- https://twitter.com/redFrik/status/839296011982024704
let a x = lfSaw AR x 0
    t = a (mce [0.5,0.6])
    f = (a 5 * a 0.015 + 1) * 98
    m = (2 ** a 4 `roundTo` 0.5) * 99
    g = grainFM 1 t 16 f m (2 ** a (1 / mce [8,9]) * 8) 0 (-1) 512
in tanh (g / 2) * 0.25
