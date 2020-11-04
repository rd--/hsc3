-- oteyPianoStrings
let k = control KR
    freq = k "freq" 440
    gate_ = k "gate" 1
    amp = k "amp" 0.5
    rho = k "rho" 1
    env = envGen AR gate_ 1 0 1 RemoveSynth (envASR 0 1 0.1 EnvLin)
    loc = linLin freq (midiCPS 36) (midiCPS 90) (-0.75) 0.75
    s = X.oteyPianoStrings AR freq amp 0 0.35 2 4 8 1 7e-2 1.4 (-4) 4 rho 1 1 0 1 0.2 1 1 0.142 1 3e-4 1
in X.oteySoundBoard AR (pan2 (s * env) loc 0.1) 20 20 0.8
