-- dwgClarinet3
let freq = control_md KR "freq" 440 (220,880,"exp",0,"")
    att = 0.01
    amp = linLin (control_md KR "amp" 0.5 (0,1,"amp",0,"")) 0 1 0.76 1
    ampn = 1
    pc = control_md KR "pc" 1 (0.25,1.05,"lin",0,"")
    m = control_md KR "m" 0.8 (0.4,1.25,"lin",0,"")
    gate_ = control_md KR "gate" 1 (0,1,"tr",1,"")
    release = 0.01
    c1 = control_md KR "c1" 0.2 (0,1,"lin",0,"")
    c3 = control_md KR "c3" 7 (5,9,"lin",0,"")
    pan = control_md KR "pan" 0 (-1,1,"lin",0,"")
    vib = sinOsc KR 4 0 * 0.001 + 1
    pm = envGen AR gate_ 1 0 1 DoNothing (envASR att 1 0.2 (EnvNum 1)) * amp
    signal = X.dwgClarinet3 AR (lag freq 0 * vib) pm pc m 1 release c1 c3
in pan2 (hpf signal 200) pan amp
