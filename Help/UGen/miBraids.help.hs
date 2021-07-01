-- MiBraids ; 6:SAW_SUB
X.miBraids ar 60 0.5 0.5 (X.miBraids_mode "SAW_SUB") 0 0 0 0 0 * 0.1

-- MiBraids ; 1:MORPH ; some modulation
let timb = lfNoise1Id 'α' kr 0.5 * 0.5 + 0.5
in X.miBraids ar 40 timb 0 (X.miBraids_mode "MORPH") 0 0 0 0 0 * 0.05

-- MiBraids ; 21:VOSIM
let pit = roundE (range 33 66 (lfNoise0Id 'α' kr 4))
    timb = lfNoise1Id 'α' kr 0.3 * 0.5 + 0.5
    color = lfNoise1Id 'α' kr 0.3 * 0.5 + 0.5
in X.miBraids ar pit timb color (X.miBraids_mode "VOSIM") 0 0 0 0 0 * 0.1

-- MiBraids ; 21:VOSIM ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let md = X.miBraids_mode "VOSIM"
          mnn = x * 24 + 36 -- x * 12 + 78
      in pan2 (X.miBraids ar mnn (y * 0.75) rx md 0 0 0 0 0) (o * 2 - 1) (g * z)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- MiBraids ; 31:FLUTED
let pit = 38;
    timb = mouseX kr 0.7 1 Linear 0.2
    color = mouseY kr 0 1 Linear 0.2
in X.miBraids ar pit timb color (X.miBraids_mode "FLUTED") 0 1 0 0 0 * 0.1

-- MiBraids ; scanning
let timb = lfNoise1Id 'α' kr 0.3 * 0.5 + 0.5
    color = lfNoise1Id 'β' kr 0.3 * 0.5 + 0.5
    pit = mouseY kr 33 73 Linear 0.2
    model = mouseX kr 0 47 Linear 0.2
in X.miBraids ar pit timb color model 0 0 0 0 0 * 0.1

-- MiBraids ; 40:WAVE_PARAPHONIC
let timb = lfNoise1Id 'α' kr 0.03 * 0.5 + 0.5
    color = lfNoise1Id 'β' kr 0.05 * 0.5 + 0.5 -- chord
in X.miBraids ar 38 timb color (X.miBraids_mode "WAVE_PARAPHONIC") 0 1 0 0 0 * 0.1

-- MiBraids ; trigger (28:PLUCKED)
let tr = dustId 'α' kr 0.6
    pit = roundE (tRandId 'β' 45 72 tr)
    timb = 0.5
    color = lfNoise1Id 'γ' kr 0.3 * 0.5 + 0.5
in X.miBraids ar pit timb color (X.miBraids_mode "PLUCKED") tr 0 0 0 0 * 0.1

-- MiBraids ; 28:PLUCKED ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let tr = trig1 g controlDur
          pit = x * 24 + 45
          timb = latch y tr * 0.5 + 0.25
          color = latch rx tr * 0.25 + 0.65
          md = X.miBraids_mode "PLUCKED"
      in pan2 (X.miBraids ar pit timb color md tr 0 0 0 0) (o * 2 - 1) (trig1 g 16 * 0.1)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- MiBraids ; 34:KICK
let tr = impulse kr 4 0
    pit = roundE (range 30 50 (latch (pinkNoiseId 'α' kr) tr))
    timb = lfNoise1Id 'β' kr 0.4 * 0.5 + 0.5
    color = lfNoise1Id 'γ' kr 0.3 * 0.5 + 0.5
in X.miBraids ar pit timb color (X.miBraids_mode "KICK") tr 0 0 0 0 * 0.2

-- MiBraids ; sample rate, bit reduction and distortion
let tr = coinGateId 'α' 0.3 (impulse kr 4 0)
    decim = tRandId 'β' 1 32 tr
    dist = range 0 1 (lfTri kr 0.2 0)
in X.miBraids ar 40 0.7 0.7 (X.miBraids_mode "KICK") tr 2 decim 3 dist * 0.1
