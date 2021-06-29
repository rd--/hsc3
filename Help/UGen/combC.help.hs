-- combC ; c.f. combN
let n = whiteNoise 'α' ar * 0.02
    dt = xLine kr 0.0001 0.01 20 RemoveSynth
in combC n 0.01 dt 0.2

-- combC ; with negative feedback
let n = whiteNoise 'α' ar * 0.02
    dt = xLine kr 0.0001 0.01 20 RemoveSynth
in combC n 0.01 dt (-0.2)

-- combC ; as an echo
let d = dust 'α' ar 1
    n = whiteNoise 'β' ar * 0.1
    i = decay d 0.2 * n
in combC i 0.2 0.2 3
