-- dwgClarinet3
let freq = control_m KR "freq" 440 (220,880,"exp")
    att = 0.01
    amp = linLin (control_m KR "amp" 0.5 (0,1,"amp")) 0 1 0.76 1
    ampn = 1
    pc = control_m KR "pc" 1 (0.25,1.05,"lin")
    m = control_m KR "m" 0.8 (0.4,1.25,"lin")
    gate_ = control_m KR "gate" 1 (0,1,"switch")
    release = 0.01
    c1 = control_m KR "c1" 0.2 (0,1,"lin")
    c3 = control_m KR "c3" 7 (5,9,"lin")
    pan = control_m KR "pan" 0 (-1,1,"lin")
    vib = sinOsc KR 4 0 * 0.001 + 1
    pm = envGen AR gate_ 1 0 1 DoNothing (envASR att 1 0.2 (EnvNum 1)) * amp
    signal = X.dwgClarinet3 AR (lag freq 0 * vib) pm pc m 1 release c1 c3
in pan2 (hpf signal 200) pan amp
