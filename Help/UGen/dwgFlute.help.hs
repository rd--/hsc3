-- dwgFlute
let freq = control_md KR "freq" 440 (220,880,"exp",0,"")
    att = 0.01
    amp = control_md KR "amp" 0.25 (0,1,"amp",0,"")
    endr = 0.5
    jetr = control_md KR "jetr" 0.24 (0,1,"lin",0,"")
    jetRa = control_md KR "jetRa" 0.33 (0,1,"lin",0,"")
    gate_ = control_md KR "gate" 1 (0,1,"tr",1,"")
    release = 0.01
    noisegain = control_md KR "noisegain" 12 (1,36,"lin",0,"")
    pan = control_md KR "pan" 0 (-1,1,"lin",0,"")
    vib = sinOsc KR 4 0 + (0.001 * noisegain * whiteNoise 'α' AR)
    pm = envGen AR gate_ 1 0 1 DoNothing (envASR att 1 0.2 (EnvNum 1)) * (1.1 + (amp * 0.2)) * vib
    signal = X.dwgFlute AR freq pm endr jetr jetRa 1 release
in pan2 (hpf (lpf signal 6000) 200) pan (0.2 * amp)
