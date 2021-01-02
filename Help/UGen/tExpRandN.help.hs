-- tExpRandN ; n sine tones, set to exponentially-distributed random frequencies on trigger
let n = 12
    tr = impulse KR 1 0
in splay (sinOsc AR (X.tExpRandN n 'α' 440 880 tr) 0) 1 0.1 0 True
