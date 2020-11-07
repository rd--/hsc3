-- twoTube
let dly1 = 100
    dly2 = 40
    env = envelope [1,1,0] [(dly1 + dly2) / sampleRate,0.0] [EnvLin]
    x = mouseX KR (-1) 1 Linear 0.2
    y = mouseY KR 1 4 Linear 0.2
    src = whiteNoise 'α' AR * envGen AR (impulse KR y 0) 1 0 1 DoNothing env
in X.twoTube AR src x 0.99 dly1 dly2 * 0.5

-- twoTube
let tr = impulse KR (mouseX KR 1 16 Linear 0.2) 0
    dly1 = tRand 'α' 1 300 tr
    dly2 = tRand 'β' 1 300 tr
    loss = tRand 'γ' 0.9 0.999 tr
    dur = tRand 'δ' 0.1 5 tr
    pan = tRand 'ε' (-1) 1 tr
    k = tRand 'ζ' (-1) 1 tr
    env = envelope [1,1,0,0] [(dly1 + dly2) / sampleRate,0,1] [EnvLin]
    src = whiteNoise 'η' AR * envGen AR tr 1 0 1 DoNothing env
in pan2 (X.twoTube AR src k loss dly1 dly2) pan (mouseY KR 0.1 0.4 Exponential 0.2)
