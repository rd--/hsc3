-- MiBraids ; 6:SAW_SUB
X.miBraids AR 60 0.5 0.5 (X.miBraids_mode_err "SAW_SUB") 0 0 0 0 0 * 0.1

-- MiBraids ; 1:MORPH ; some modulation
let timb = lfNoise1 'α' KR 0.5 * 0.5 + 0.5
in X.miBraids AR 40 timb 0 (X.miBraids_mode_err "MORPH") 0 0 0 0 0 * 0.05

-- MiBraids ; 21:VOSIM
let pit = roundE (range 33 66 (lfNoise0 'α' KR 4))
    timb = lfNoise1 'α' KR 0.3 * 0.5 + 0.5
    color = lfNoise1 'α' KR 0.3 * 0.5 + 0.5
in X.miBraids AR pit timb color (X.miBraids_mode_err "VOSIM") 0 0 0 0 0 * 0.1

-- MiBraids ; 31:FLUTED
let pit = 38;
    timb = mouseX KR 0.7 1 Linear 0.2
    color = mouseY KR 0 1 Linear 0.2
in X.miBraids AR pit timb color (X.miBraids_mode_err "FLUTED") 0 1 0 0 0 * 0.1

-- MiBraids ; scanning
let timb = lfNoise1 'α' KR 0.3 * 0.5 + 0.5
    color = lfNoise1 'β' KR 0.3 * 0.5 + 0.5
    pit = mouseY KR 33 73 Linear 0.2
    model = mouseX KR 0 47 Linear 0.2
in X.miBraids AR pit timb color model 0 0 0 0 0 * 0.1

-- MiBraids ; 40:WAVE_PARAPHONIC
let timb = lfNoise1 'α' KR 0.03 * 0.5 + 0.5
    color = lfNoise1 'β' KR 0.05 * 0.5 + 0.5 -- chord
in X.miBraids AR 38 timb color (X.miBraids_mode_err "WAVE_PARAPHONIC") 0 1 0 0 0 * 0.1

-- MiBraids ; trigger (28:PLUCKED)
let tr = dust 'α' KR 0.6
    pit = roundE (tRand 'β' 45 72 tr)
    timb = 0.5
    color = lfNoise1 'γ' KR 0.3 * 0.5 + 0.5
in X.miBraids AR pit timb color (X.miBraids_mode_err "PLUCKED") tr 0 0 0 0 * 0.1

-- MiBraids ; 34:KICK
let tr = impulse KR 4 0
    pit = roundE (range 30 50 (latch (pinkNoise 'α' KR) tr))
    timb = lfNoise1 'β' KR 0.4 * 0.5 + 0.5
    color = lfNoise1 'γ' KR 0.3 * 0.5 + 0.5
in X.miBraids AR pit timb color (X.miBraids_mode_err "KICK") tr 0 0 0 0 * 0.2

-- MiBraids ; sample rate, bit reduction and distortion
let tr = coinGate 'α' 0.3 (impulse KR 4 0)
    decim = tRand 'β' 1 32 tr
    dist = range 0 1 (lfTri KR 0.2 0)
in X.miBraids AR 40 0.7 0.7 (X.miBraids_mode_err "KICK") tr 2 decim 3 dist * 0.1
