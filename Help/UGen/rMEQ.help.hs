-- rMEQ ; default param ; rm=regalia-mitra freq=cut-off frequency (hz) rq=reciprocal-of-Q k=gain (db)
X.rMEQ AR (pinkNoise 'α' AR * 0.1) 440 1 0

-- rMEQ ; rapid param changes fail at low frequencies
let freq = mouseX KR 55 3520 Exponential 0.2
    rq = mouseY KR 0.01 2.0 Linear 0.2
    k = 3
in X.rMEQ AR (pinkNoise 'α' AR * 0.1) freq rq k
