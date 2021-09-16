-- dwgClarinet3
let freq = control_m kr "freq" 440 (220,880,"exp")
    att = 0.01
    amp = linLin (control_m kr "amp" 0.5 (0,1,"amp")) 0 1 0.76 1
    ampn = 1
    pc = control_m kr "pc" 1 (0.25,1.05,"lin")
    m = control_m kr "m" 0.8 (0.4,1.25,"lin")
    gate_ = control_m kr "gate" 1 (0,1,"switch")
    release = 0.01
    c1 = control_m kr "c1" 0.2 (0,1,"lin")
    c3 = control_m kr "c3" 7 (5,9,"lin")
    pan = control_m kr "pan" 0 (-1,1,"lin")
    vib = sinOsc kr 4 0 * 0.001 + 1
    pm = envGen ar gate_ 1 0 1 DoNothing (envASR att 1 0.2 (EnvNum 1)) * amp
    signal = X.dwgClarinet3 ar (lag freq 0 * vib) pm pc m 1 release c1 c3
in pan2 (hpf signal 200) pan amp

-- dwgClarinet3 ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCps (x * 25 + 42)
          vib = sinOsc kr (y * 4) 0 * (z * 0.005) + 1
          amp = linLin z 0 1 0.65 1
          pm = k2a (amp * g)
          pc = linLin y 0 1 0.25 0.85
          m = linLin rx 0 1 0.4 1.25
          c1 = ry
          c3 = linLin o 0 1 5 9
          signal = X.dwgClarinet3 ar (freq * vib) pm pc m 1 0.01 c1 c3
      in pan2 (hpf signal 200) (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 0.25
