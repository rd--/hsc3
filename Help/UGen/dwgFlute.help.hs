-- dwgFlute
let freq = control_m KR "freq" 440 (220,880,"exp")
    att = 0.01
    amp = control_m KR "amp" 0.25 (0,1,"amp")
    endr = 0.5
    jetr = control_m KR "jetr" 0.24 (0,1,"lin")
    jetRa = control_m KR "jetRa" 0.33 (0,1,"lin")
    gate_ = control_m KR "gate" 1 (0,1,"switch")
    release = 0.01
    noisegain = control_m KR "noisegain" 12 (1,36,"lin")
    pan = control_m KR "pan" 0 (-1,1,"lin")
    vib = sinOsc KR 4 0 + (0.001 * noisegain * whiteNoise 'α' AR)
    pm = envGen AR gate_ 1 0 1 DoNothing (envASR att 1 0.2 (EnvNum 1)) * (1.1 + (amp * 0.2)) * vib
    signal = X.dwgFlute AR freq pm endr jetr jetRa 1 release
in pan2 (hpf (lpf signal 6000) 200) pan (0.2 * amp)
