-- bufGrain ; requires=buf
let x = mouseX kr 0.5 8 Linear 0.2
    y = mouseY kr 0.05 0.2 Linear 0.2
    e = envGen kr 1 1 0 1 RemoveSynth (envelope [0,1,0] [3,2] [EnvSin,EnvSin])
in X.bufGrain ar (impulse kr 10 0) y (control kr "buf" 0) x e 2
