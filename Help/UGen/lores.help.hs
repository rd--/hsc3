-- lores
X.lores (whiteNoise 'α' AR * 0.5) 880 0.5

-- lores ; modulate param
let src = whiteNoise 'α' AR * 0.3
    freq = lfNoise0 'β' AR 4 * 500 + 600
    res = 0.9
in X.lores src freq res
