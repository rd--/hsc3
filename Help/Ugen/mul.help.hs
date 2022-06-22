-- mul
sinOsc ar 440 0 * 0.15

-- mul ; creates a beating effect (subaudio rate)
fSinOsc kr 10 0 * pinkNoiseId 'α' ar * 0.1

-- mul ; ring modulation
let p = sinOsc ar (xLine kr 100 1001 10 DoNothing) 0
    q = syncSaw ar 100 200
in p * q * 0.25

-- mul ; optimises identity
sinOsc ar 440 0 * 1 / 10

-- mul ; optimises identity
1 * sinOsc ar 440 0 / 10

-- mul ; gate ; control
let freq = control_m kr "freq" 220 (55,880,"exp")
    amp = control_m kr "amp" 0.1 (0,1,"amp")
    gate_ = control_m kr "gate" 0 (0,1,"switch")
in sinOsc ar freq 0 * amp * lag gate_ 0.1
