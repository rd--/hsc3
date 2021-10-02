-- stkModalBar ; https://ccrma.stanford.edu/software/stk/classstk_1_1ModalBar.html
let x = mouseX kr 0.25 12 Linear 0.2
    tr = impulse kr x 0 - 0.5
    freq = midiCps (tiRandId 'α' 25 96 tr)
    instrument = tiRandId 'β' 0 9 tr -- Marimba, Vibraphone, Agogo, Wood1, Reso, Wood2, Beats, Two Fixed, Clump
    stickhardness = tRandId 'γ' 0 127 tr
    stickposition = tRandId 'δ' 0 127 tr
    vibratogain = tRandId 'ε' 0 127 tr
    vibratofreq = tRandId 'ζ' 0 127 tr
    directstickmix = tRandId 'η' 0 127 tr
    volume = tRandId 'θ' 0 127 tr
in X.stkModalBar ar freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume tr

-- stkModalBar
let x = mouseX kr 1 12 Linear 0.2
    tr = impulse kr x 0 - 0.5
    tr3 = pulseDivider tr 3 0
    freq = midiCps (tiRandId 'α' 52 64 tr)
    instrument = 1
    stickhardness = tRandId 'β' 10 50 tr3
    stickposition = tRandId 'γ' 40 80 tr3
    vibratogain = tRandId 'δ' 66 98 tr3
    vibratofreq = tRandId 'ε' 4 12 tr3
    directstickmix = tRandId 'ζ' 0 1 tr3
    volume = tRandId 'η' 16 48 tr3
in X.stkModalBar ar freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume tr

-- stkModalBar ; event control
let f (_,g,_,y,z,o,rx,_,p,_,_) =
      let tr = g - 0.5
          freq = midiCps p
          instr = control kr "instrument" 4
          sig = X.stkModalBar ar freq instr ((1 - y) * 127) (y * 127) 64 64 (rx * 127) 127 tr
      in pan2 sig (o * 2 - 1) (latch z g * 3)
in mix (eventVoicer 16 f) * control kr "gain" 1
