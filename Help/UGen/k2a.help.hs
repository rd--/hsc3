-- k2a
k2a (whiteNoise 'α' KR * 0.3)

-- k2a ; compare
mce2 (k2a (whiteNoise 'α' KR * 0.3)) (whiteNoise 'β' AR * 0.1)

-- k2a
let freq = (mouseX KR 0.1 40 Exponential 0.2) / blockSize * sampleRate;
    o1 = k2a (lfNoise0 'α' KR freq)
    o2 = lfNoise0 'β' AR freq
in mce2 o1 o2 * 0.1
