-- resonator
let pan = 0
    trig = impulse kr 1 0
    snare = X.analogSnareDrum ar trig 0 0.1 200 0.5 (tRand 'α' 0.1 0.5 trig) 0.5
    freq = tExpRand 'β' 25 250 trig
    position = 0
    resolution = 24
    structure = sinOsc kr 0.01 0 `in_range` (0,1)
    brightness = sinOsc kr 0.01 (0.5 * pi) `in_range` (0,0.5)
    damping = tRand 'γ' 0.2 0.8 trig
    sig = X.resonator snare freq position resolution structure brightness damping
in pan2 (tanh sig) pan 0.1

-- resonator ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let tr = trig g controlDur
          freq = midiCPS (x * 25 + 36)
          snare = X.analogSnareDrum ar tr 0 z 200 0.5 (tRand 'α' 0.1 0.5 tr) 0.5
          sig = X.resonator snare freq 0 24 y rx (0.2 + ry)
      in pan2 (tanh sig) (o * 2 - 1) (lagUD g 0 6)
in mix (eventVoicer 16 f) * control kr "gain" 0.1

-- resonator ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 25 + 36)
          sig = X.resonator (pinkNoise 'α' ar * z) freq 0 24 y rx (0.2 + ry)
      in pan2 (tanh sig) (o * 2 - 1) (lagUD g 0 2)
in mix (eventVoicer 16 f) * control kr "gain" 0.1
