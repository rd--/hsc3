-- linPan2
let n = pinkNoise 'α' ar in linPan2 n (fSinOsc kr 2 0) 0.05

-- linPan2
linPan2 (fSinOsc ar 800 0) (fSinOsc kr 3 0) 0.05
