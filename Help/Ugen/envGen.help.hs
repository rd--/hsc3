-- envGen ; https://www.listarc.bham.ac.uk/lists/sc-users/msg14815.html
let n = range 0.01 0.15 (lfNoise1Id 'α' kr 2)
    e = Envelope [0,1] [n] [EnvLin] Nothing (Just 0) 0
    c = env_circle_0 e
    a = envGen ar 1 1 0 1 DoNothing c
in sinOsc ar (a * 400 + 500) 0 * 0.1

-- envGen ; env_circle joins the end of the envelope to the start
let e = envelope [6000,700,100] [1,1] [EnvExp,EnvLin]
    c = env_circle_z 0 1 EnvLin e
    f = envGen kr 1 1 0 1 DoNothing c
in (sinOsc ar f 0 + impulse ar 1 0) * 0.1

-- envGen ; c.f. envXYC ; non-linear Phasor ; positive half traversed more quickly than negative
let e = envXYC [(0,0,EnvNum (-0.5)),(0.4,pi,EnvNum 0.5),(1,two_pi,EnvLin)]
    o = sinOsc kr 0 (envGen kr 1 1 0 2 DoNothing (env_circle_0 e))
in (soundIn 0 + pinkNoiseId 'α' ar * 0.1) * range 0.25 1 o

-- envGen ; impulse train with curvature ; see below for control messages ; https://fredrikolofsson.com/f0blog/impulse-train/
let go = trigControl "go" 1 -- trigger
    dur = control kr "dur" 1 -- total duration in seconds (also frequency)
    cur = control kr "cur" 0 -- curvature or 'shape'. 0 = pulses at regular intervals
    num = control kr "num" 8 -- number of impulses per duration
    amp = control kr "amp" 1 -- gain
    env = envGen ar go 1 0 1 DoNothing (envelope [0, 0, 1] [0, dur] [EnvNum cur])
in changed (ceil (env * num)) 0 * amp

{---- ; see also help files for the following envelope constructors

- envADSR
- envASR
- envCoord
- envGate
- envLinen
- envPairs
- envPerc
- envSine
- envStep
- envTrapezoid
- envTriangle
- envXYC

-}

---- ; set impulse train controls
set = withSc3 . sendMessage . n_set (-1)
set [("go", 1), ("cur", 3), ("num", 18)] -- start slow and go faster
set [("go", 1), ("cur", -2), ("num", 18)] -- slower and slower
set [("go", 1), ("amp", 0.2), ("cur", 0), ("dur", 0.5), ("num", 200)] -- 200 for half a second = 400 Hz

---- ; drawings
let e = envelope [6000,700,100] [1,1] [EnvExp,EnvLin]
envelope_sc3_array e == Just [6000,2,-99,-99,700,1,2,0,100,1,1,0]
Sound.Sc3.Plot.plotEnvelope [e]
