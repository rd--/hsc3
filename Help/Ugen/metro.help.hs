-- metro
X.metro ar 60 1

-- metro
let b = xLine kr 60 120 5 DoNothing
    m = X.metro kr b 1
    o = sinOsc ar 440 0 * 0.1
in decay m 0.2 * o

-- metro
let b = range 30 240 (lfNoise2Id 'α' kr 0.2)
    n = dseqId 'β' dinf (mce [1,0.25,0.5,0.25])
in decay (X.metro kr b n) 0.2 * sinOsc ar 440 0 * 0.1
