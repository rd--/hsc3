-- linPan2
let n = pinkNoise 'α' AR in linPan2 n (fSinOsc KR 2 0) 0.05

-- linPan2
linPan2 (fSinOsc AR 800 0) (fSinOsc KR 3 0) 0.05
