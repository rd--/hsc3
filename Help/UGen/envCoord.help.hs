-- envCoord ; co-ordinate (break-point) envelope
let c = EnvLin
    p = envCoord [(0,0),(0.5,0.1),(0.55,1),(1,0)] 9 0.1 c
    e = envGen kr 1 1 0 1 RemoveSynth p
in sinOsc ar 440 0 * e

-- envCoord ; line segments ; internal graph triggers and randomises line end points
let tr = dust 'α' kr 2
    st = 440
    en = tRand 'β' 300 900 tr
    tm = tRand 'γ' 0.5 1.5 tr
    p = envCoord [(0,st),(tm,en)] 1 1 EnvLin
    e = envGen kr tr 1 0 1 DoNothing p
in sinOsc ar e 0 * 0.2

-- envCoord ; line segments ; external control
let tr = tr_control "tr" 1
    st = control kr "st" 440
    en = control kr "en" 880
    tm = control kr "tm" 2
    p = envCoord [(0,st),(tm,en)] 1 1 EnvLin
    e = envGen kr tr 1 0 1 DoNothing p
in sinOsc ar e 0 * 0.2

-- envCoord ; start and end reset randomly every 4 seconds, ramp every trigger
let tr = impulse kr (1/4) 0
    f0 = tExpRand 'α' 110 450 tr
    f1 = tExpRand 'β' 110 440 tr
    p = envCoord [(0,0),(0,f0),(4,f1)] 1 1 EnvExp
    e = envGen kr tr 1 0 1 DoNothing p
in splay (sinOsc ar (mce [f0,e,f1]) 0) 1 1 0 True * mce [0.05,0.1,0.05]

---- ; set target value & transition time and trigger
withSC3 (sendMessage (n_set 1 [("en",550),("tm",4),("tr",1)]))
withSC3 (sendMessage (n_set 1 [("en",990),("tm",1),("tr",1)]))
withSC3 (sendMessage (n_set 1 [("en",110),("tm",2),("tr",1)]))

---- ; plotting
Sound.SC3.Plot.plotEnvelope [envCoord [(0,0),(0.35,0.1),(0.55,1),(1,0)] 9 0.1 EnvLin]
Sound.SC3.Plot.plotEnvelope [envCoord [(0,0),(0.15,0.6),(0.35,0.2),(1,0)] 6 0.1 EnvSin]
Sound.SC3.Plot.plotEnvelope [envCoord [(0,0),(0.65,0.3),(0.85,0.7),(1,0)] 5 0.1 EnvCub]
Sound.SC3.Plot.plotEnvelope [envCoord [(0,0.1),(0.25,0.6),(0.5,0.4),(1,0.4)] 7 0.1 EnvStep]
Sound.SC3.Plot.plotEnvelope [envCoord (zip [-1,-0.7,0.7,1] [0,0.45,0.55,1]) 1 1 EnvLin]
