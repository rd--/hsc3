-- tXLine ; start and end reset randomly every 4 seconds, ramp every trigger
let tr = impulse KR (1/4) 0
    f0 = tExpRand 'α' 110 450 tr
    f1 = tExpRand 'β' 110 440 tr
in splay (sinOsc AR (mce [f0,tXLine KR f0 f1 4 tr,f1]) 0) 1 1 0 True * mce [0.05,0.1,0.05]
