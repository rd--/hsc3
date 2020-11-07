-- fdiv
let o = fSinOsc KR 10 0.5
    n = pinkNoise 'α' AR
in (n * 0.1) / (o * 0.75)
