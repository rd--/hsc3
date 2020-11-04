-- sos ; same as twopole
let theta = line KR (0.2 * pi) pi 5 RemoveSynth
    rho = line KR 0.6 0.99 5 RemoveSynth
    b1 = 2 * rho * cos theta
    b2 = - (rho * rho)
in sos (lfSaw AR 200 0 * 0.1) 1 0 0 b1 b2

-- sos ; http://www.earlevel.com/main/2011/01/02/biquad-formulas/
let fc = 100
    sr = 48000
    k = tan (pi * fc / sr)
    q = 0.707
    norm = 1 / (1 + k / q + k * k)
    a0 = k * k * norm
    a1 = 2 * a0
    a2 = a0
    b1 = 2 * (k * k - 1) * norm
    b2 = (1 - k / q + k * k) * norm
in sos (whiteNoise 'α' AR * 0.2) a0 a1 a2 (- b1) (- b2)
