-- stkModalBar ; https://ccrma.stanford.edu/software/stk/classstk_1_1ModalBar.html
let x = mouseX KR 0.25 12 Linear 0.2
    tr = impulse KR x 0 - 0.5
    freq = midiCPS (tiRand 'α' 25 96 tr)
    instrument = tiRand 'β' 0 9 tr -- Marimba, Vibraphone, Agogo, Wood1, Reso, Wood2, Beats, Two Fixed, Clump
    stickhardness = tRand 'γ' 0 127 tr
    stickposition = tRand 'δ' 0 127 tr
    vibratogain = tRand 'ε' 0 127 tr
    vibratofreq = tRand 'ζ' 0 127 tr
    directstickmix = tRand 'η' 0 127 tr
    volume = tRand 'θ' 0 127 tr
in X.stkModalBar AR freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume tr

-- stkModalBar
let x = mouseX KR 1 12 Linear 0.2
    tr = impulse KR x 0 - 0.5
    tr3 = pulseDivider tr 3 0
    freq = midiCPS (tiRand 'α' 52 64 tr)
    instrument = 1
    stickhardness = tRand 'β' 10 50 tr3
    stickposition = tRand 'γ' 40 80 tr3
    vibratogain = tRand 'δ' 66 98 tr3
    vibratofreq = tRand 'ε' 4 12 tr3
    directstickmix = tRand 'ζ' 0 1 tr3
    volume = tRand 'η' 16 48 tr3
in X.stkModalBar AR freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume tr

-- stkModalBar ; event control
let f _ (g,_,y,z,o,rx,_,p) =
      let tr = g - 0.5
          freq = midiCPS p
          instr = control KR "instrument" 4
          sig = X.stkModalBar AR freq instr ((1 - y) * 127) (y * 127) 64 64 (rx * 127) 127 tr
      in pan2 sig (o * 2 - 1) (latch z g * 3)
in mix (rEventVoicer 16 f) * control KR "gain" 1
