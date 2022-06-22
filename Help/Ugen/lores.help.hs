-- lores
X.lores (whiteNoiseId 'α' ar * 0.5) 880 0.5

-- lores ; modulate param
let src = whiteNoiseId 'α' ar * 0.3
    freq = lfNoise0Id 'β' ar 4 * 500 + 600
    res = 0.9
in X.lores src freq res
