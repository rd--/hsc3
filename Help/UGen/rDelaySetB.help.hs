-- rDelaySetB ; three one second delays
let x = mouseX kr 110 660 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
    s = sinOsc ar x 0 * y
    b = clearBuf (localBuf 'α' 1 (5 * 44100))
    d = X.rDelaySetB b s (mce [1,1,1/5,2,1/2,1/10,3,1/3,1/15])
in mce2 s d * 0.1
