-- twoTube
let dly1 = 100
    dly2 = 40
    env = envelope [1,1,0] [(dly1 + dly2) / sampleRate,0.0] [EnvLin]
    x = mouseX kr (-1) 1 Linear 0.2
    y = mouseY kr 1 4 Linear 0.2
    src = whiteNoise 'α' ar * envGen ar (impulse kr y 0) 1 0 1 DoNothing env
in X.twoTube ar src x 0.99 dly1 dly2 * 0.5

-- twoTube
let tr = impulse kr (mouseX kr 1 16 Linear 0.2) 0
    dly1 = tRand 'α' 1 300 tr
    dly2 = tRand 'β' 1 300 tr
    loss = tRand 'γ' 0.9 0.999 tr
    dur = tRand 'δ' 0.1 5 tr
    pan = tRand 'ε' (-1) 1 tr
    k = tRand 'ζ' (-1) 1 tr
    env = envelope [1,1,0,0] [(dly1 + dly2) / sampleRate,0,1] [EnvLin]
    src = whiteNoise 'η' ar * envGen ar tr 1 0 1 DoNothing env
in pan2 (X.twoTube ar src k loss dly1 dly2) pan (mouseY kr 0.1 0.4 Exponential 0.2)
