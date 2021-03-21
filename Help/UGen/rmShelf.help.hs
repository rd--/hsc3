-- rmShelf2 ; default parameters ; rm=regalia-mitra freq=cut-off frequency (hz) k=gain (db)
X.rmShelf2 AR (whiteNoise 'α' AR * 0.1) 440 0

-- rmShelf2 ; freq=mouse-x
let freq = mouseX KR 55 3520 Exponential 0.2
in X.rmShelf2 AR (whiteNoise 'α' AR * 0.1) freq 0

-- rmShelf2 ; k=mouse-y
let freq = mouseX KR 55 3520 Exponential 0.2
    k = mouseY KR (-12) 12 Linear 0.2
in X.rmShelf2 AR (whiteNoise 'α' AR * 0.1) freq k * 0.1

