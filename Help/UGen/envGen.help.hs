-- envGen ; https://www.listarc.bham.ac.uk/lists/sc-users/msg14815.html
let n = range 0.01 0.15 (lfNoise1 'α' KR 2)
    e = Envelope [0,1] [n] [EnvLin] Nothing (Just 0) 0
    c = env_circle_0 e
    a = envGen AR 1 1 0 1 DoNothing c
in sinOsc AR (a * 400 + 500) 0 * 0.1

-- envGen ; env_circle joins the end of the envelope to the start
let e = envelope [6000,700,100] [1,1] [EnvExp,EnvLin]
    c = env_circle_z 0 1 EnvLin e
    f = envGen KR 1 1 0 1 DoNothing c
in (sinOsc AR f 0 + impulse AR 1 0) * 0.1

-- envGen ; c.f. envXYC ; non-linear Phasor ; positive half traversed more quickly than negative
let e = envXYC [(0,0,EnvNum (-0.5)),(0.4,pi,EnvNum 0.5),(1,two_pi,EnvLin)]
    o = sinOsc KR 0 (envGen KR 1 1 0 2 DoNothing (env_circle_0 e))
in (soundIn 0 + pinkNoise 'α' AR * 0.1) * range 0.25 1 o

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

---- ; drawings
let e = envelope [6000,700,100] [1,1] [EnvExp,EnvLin]
envelope_sc3_array e == Just [6000,2,-99,-99,700,1,2,0,100,1,1,0]
Sound.SC3.Plot.plotEnvelope [e]
