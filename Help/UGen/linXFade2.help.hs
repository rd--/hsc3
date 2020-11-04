-- linXFade2 ; SC3 has multiplier pseudo input
linXFade2 (fSinOsc AR 800 0 * 0.1) (pinkNoise 'α' AR * 0.1) (fSinOsc KR 1 0) * 1

-- linXFade2
linXFade2 (saw AR 440) (sinOsc AR 440 0) (lfTri KR 0.1 0) * 0.05
