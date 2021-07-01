-- packFFT
let b = localBufId 'α' 512 1
    n = 100
    square a = a * a
    r1 z = range 0 1 (fSinOsc kr (expRandId (z,'β') 0.1 1) 0)
    m1 = map r1 (id_seq n 'γ')
    m2 = zipWith (*) m1 (map square [1.0, 0.99 ..])
    r2 z = lfPulse kr (2 ** iRandId (z,'δ') (-3) 5) 0 0.3
    i = map r2 (id_seq n 'ε')
    m3 = zipWith (*) m2 i
    p = replicate n 0.0
    c1 = fft' b (fSinOsc ar 440 0)
    c2 = packFFT c1 512 0 (n - 1) 1 (packFFTSpec m3 p)
    s = ifft' c2
in mce2 s s
