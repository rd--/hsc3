-- linXFade2 ; SC3 has multiplier pseudo input
linXFade2 (fSinOsc ar 800 0 * 0.1) (pinkNoise 'α' ar * 0.1) (fSinOsc kr 1 0) * 1

-- linXFade2
linXFade2 (saw ar 440) (sinOsc ar 440 0) (lfTri kr 0.1 0) * 0.05
