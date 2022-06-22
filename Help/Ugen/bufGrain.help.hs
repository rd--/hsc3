-- bufGrain ; requires=buf
let b = control kr "buf" 0
    x = mouseX kr 0.5 8 Linear 0.2
    y = mouseY kr 0.05 0.2 Linear 0.2
    e = envGen kr 1 1 0 1 RemoveSynth (envelope [0, 1, 0] [3, 2] [EnvSin, EnvSin])
in X.bufGrain ar (impulse kr 10 0) y b x e 2

---- ; load buffer
withSC3 (async (b_allocRead 0 (sfResolve "instr/celeste/long/13-C4-long.wav") 0 0))
