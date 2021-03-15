-- mul
sinOsc AR 440 0 * 0.15

-- mul ; creates a beating effect (subaudio rate)
fSinOsc KR 10 0 * pinkNoise 'α' AR * 0.1

-- mul ; ring modulation
let p = sinOsc AR (xLine KR 100 1001 10 DoNothing) 0
    q = syncSaw AR 100 200
in p * q * 0.25

-- mul ; optimises identity
sinOsc AR 440 0 * 1 / 10

-- mul ; optimises identity
1 * sinOsc AR 440 0 / 10

-- mul ; gate ; control
let freq = control_md KR "freq" 220 (55,880,"exp",0,"")
    amp = control_md KR "amp" 0.1 (0,1,"amp",0,"")
    gate_ = control_md KR "gate" 0 (0,1,"gate",1,"")
in sinOsc AR freq 0 * amp * lag gate_ 0.1
