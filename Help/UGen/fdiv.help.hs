-- fdiv
let o = fSinOsc kr 10 0.5
    n = pinkNoise 'α' ar
in (n * 0.1) / (o * 0.75)
