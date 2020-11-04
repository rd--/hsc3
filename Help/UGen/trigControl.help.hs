-- trigControl ; graph with the three types of non-audio controls ; trigger controls are drawn cyan
let freq = control KR "freq" 440
    phase = control IR "phase" 0
    gate' = tr_control "gate" 1
    amp = control KR "amp" 0.1
    e = envGen KR gate' amp 0 1 DoNothing (envASR 0.01 1 1 EnvLin)
in sinOsc AR freq phase * e
