-- rlpf
let n = whiteNoiseId 'α' ar
    f = sinOsc ar 0.5 0 * 40 + 220
in rlpf n f 0.1

-- rlpf
let f = fSinOsc kr (xLine kr 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in rlpf (saw ar 200 * 0.1) f 0.2

-- rlpf
let ctl = rlpf (saw ar 5 * 0.1) 25 0.03
in sinOsc ar (ctl * 200 + 400) 0 * 0.1

-- rlpf ; mouse-control
let x = mouseX kr 2 200 Exponential 0.2
    y = mouseY kr 0.01 1 Exponential 0.2
    ctl = rlpf (saw ar 5 * 0.1) x y
in sinOsc ar (ctl * 200 + 400) 0 * 0.1

-- rlpf ; c.f. rlpfd
let s = mix (lfSaw ar (mce2 120 180) 0 * 0.33)
    f = linExp (lfCub kr 0.1 (0.5 * pi)) (-1) 1 280 1500
    rq = mouseX kr 0.05 0.5 Linear 0.2
in rlpf s f rq * 0.1

-- rlpf ; event control
let f (_,g,_,y,z,o,rx,ry,p,_,_) =
      let f0 = midiCps p
          f1 = f0 * 0.5 * (1 + y * 6)
          rq = linLin (rx * ry) 0 0.25 0.1 0.6
      in pan2 (rlpf (lfTri ar f0 0) f1 rq) (o * 2 - 1) (lagUD g 0.05 (2 - y) * (2 - y) * z)
in mix (eventVoicer 16 f) * control kr "gain" 0.25
