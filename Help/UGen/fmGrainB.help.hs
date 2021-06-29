-- fmGrainB
let b = control kr "tbl" 10
    t = impulse ar 20 0
    n = linLin (lfNoise1 'α' kr 1) (-1) 1 1 10
    s = envSine 9 0.1
    e = envGen kr 1 1 0 1 RemoveSynth s
in X.fmGrainB t 0.2 440 220 n b * e

---- ; alloc table
withSC3 (mapM_ maybe_async [b_alloc 10 512 1,b_gen_sine2 10 [Normalise,Wavetable,Clear] [(0.5,0.1)]])
