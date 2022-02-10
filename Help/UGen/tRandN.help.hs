-- tRandN ; n sine tones, set to uniform-random frequencies on trigger
let n = 12
    tr = impulse kr 1 0
in splay (sinOsc ar (X.tRandNId n 'α' 440 880 tr) 0) 1 0.1 0 True
