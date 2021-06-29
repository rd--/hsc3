-- k2a
k2a (whiteNoise 'α' kr * 0.3)

-- k2a ; compare
mce2 (k2a (whiteNoise 'α' kr * 0.3)) (whiteNoise 'β' ar * 0.1)

-- k2a
let freq = (mouseX kr 0.1 40 Exponential 0.2) / blockSize * sampleRate;
    o1 = k2a (lfNoise0 'α' kr freq)
    o2 = lfNoise0 'β' ar freq
in mce2 o1 o2 * 0.1
