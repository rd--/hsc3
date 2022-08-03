-- trigControl ; graph with the three types of non-audio controls ; trigger controls are drawn cyan
let freq = control kr "freq" 440
    phase = control ir "phase" 0
    gate' = trigControl "gate" 1
    amp = control kr "amp" 0.1
    e = envGen kr gate' amp 0 1 DoNothing (envAsr 0.01 1 1 EnvLin)
in sinOsc ar freq phase * e
