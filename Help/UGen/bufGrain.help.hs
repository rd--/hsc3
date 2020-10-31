-- bufGrain ; requires=buf
let x = mouseX KR 0.5 8 Linear 0.2
    y = mouseY KR 0.05 0.2 Linear 0.2
    e = envGen KR 1 1 0 1 RemoveSynth (envelope [0,1,0] [3,2] [EnvSin,EnvSin])
in X.bufGrain AR (impulse KR 10 0) y (control KR "buf" 0) x e 2
