-- MiRings ; basics ; 0=MODAL_RESONATOR
X.miRings AR (impulse AR 1 0) 0 60 0.25 0.5 0.7 0.25 (X.miRings_mode "MODAL_RESONATOR") 1 0 0 0

-- MiRings ; basics ; 0=MODAL_RESONATOR ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let tr = trig1 g controlDur
          md = X.miRings_mode "MODAL_RESONATOR"
      in X.miRings AR 0
         tr (x * 24 + 36) (latch o tr * 0.75 + 0.25) (y * 0.65 + 0.15) (0.7 - latch rx tr) (0.25 + latch ry tr)
         md 1 0 0 0 * z
in mix (rEventVoicer 16 f) * control KR "gain" 4

-- MiRings ; basics
X.miRings AR (pinkNoise 'α' AR * 0.05) 0 40 0.25 0.5 0.7 0.25 (X.miRings_mode "MODAL_RESONATOR") 1 0 0 0

-- MiRings ; using the 'trig' input to excite the resonator ; 1=SYMPATHETIC_STRING
X.miRings AR 0 (dust 'α' KR 0.7) 33 0.25 0.5 0.7 0.25 (X.miRings_mode "SYMPATHETIC_STRING") 1 0 0 0 * 0.2

-- MiRings ; basics ; 1=SYMPATHETIC_STRING ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let tr = trig1 g controlDur
          md = X.miRings_mode "SYMPATHETIC_STRING"
      in X.miRings AR 0
         tr (x * 24 + 36) (latch o tr * 0.75 + 0.25) (y * 0.65 + 0.15) (0.7 - latch rx tr) (0.25 + latch ry tr)
         md 1 0 0 0 * z
in mix (rEventVoicer 16 f) * control KR "gain" 2

-- MiRings ; using the 'pit' input to set MIDI pitch and excite the resonator
X.miRings AR 0 0 (range 30 50 (lfNoise0 'α' KR 2)) 0.25 0.5 0.7 0.25 (X.miRings_mode "SYMPATHETIC_STRING") 1 0 0 0 * 0.2

-- MiRings ; 2=MODULATED/INHARMONIC_STRING
X.miRings AR 0 0 (range 30 50 (lfNoise0 'α' KR 2)) 0.25 0.5 0.7 0.25 (X.miRings_mode "MODULATED/INHARMONIC_STRING") 1 0 0 0 * 0.2

-- MiRings ; basics ; 2=MODULATED/INHARMONIC_STRING ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let tr = trig1 g controlDur
          md = X.miRings_mode "MODULATED/INHARMONIC_STRING"
      in X.miRings AR 0
         tr (x * 24 + 36) (0.25 + latch o tr * 0.15) (0.25 + y * 0.5) (0.7 - latch rx tr * 0.15) (0.25 + latch ry tr * 0.15)
         md 1 0 0 0 * z
in mix (rEventVoicer 16 f) * control KR "gain" 2

-- MiRings ; sympathetic strings
let tr = dust 'α' KR 1
    pit = roundE (range 30 60 (latch (whiteNoise 'β' KR) tr))
    struct = range 0 1 (lfNoise2 'γ' KR 0.4)
in X.miRings AR 0 0 pit struct 0.5 0.7 0.25 (X.miRings_mode "SYMPATHETIC_STRING") 4 0 0 0 * 0.25

-- MiRings ; inharmonic string
let tr = dust 'α' KR 3
    trPit = dust 'β' KR 0.5
    pit = range 30 60 (latch (whiteNoise 'γ' KR) trPit)
    struct = range 0.1 1 (latch (pinkNoise 'δ' KR) tr)
in X.miRings AR 0 tr pit struct 0.5 0.8 0.25 (X.miRings_mode "MODULATED/INHARMONIC_STRING") 4 0 0 0 * 0.25

-- MiRings ; simple vibrato
let tr = dust 'α' KR 0.5
    pit = range 36 48 (latch (whiteNoise 'β' KR) tr)
    vib = sinOsc KR 3 0
in X.miRings AR 0 0 (pit + vib) 0.2 0.5 0.7 0.25 (X.miRings_mode "MODAL_RESONATOR") 1 0 0 0

-- MiRings ; 'intern_exciter' OFF
X.miRings AR (pinkNoise 'α' AR * 0.1) (impulse KR 1 0) 60 0.25 0.5 0.7 0.25 (X.miRings_mode "MODAL_RESONATOR") 2 0 0 0 * 0.25

-- MiRings ; 'intern_exciter' ON
X.miRings AR (pinkNoise 'α' AR * 0.1) (impulse KR 1 0) 60 0.25 0.5 0.7 0.25 (X.miRings_mode "MODAL_RESONATOR") 2 1 0 0 * 0.25

-- MiRings ; intern_exciter ON
let input = crackle AR 1.999 * 0.2
    tr = dust 'α' AR 0.7
    bright = range 0.1 0.8 (lfNoise1 'β' KR 0.4)
    pos = range 0 1 (lfNoise2 'γ' KR 0.1)
in X.miRings AR input tr 56 0.1 bright 0.6 pos (X.miRings_mode "MODAL_RESONATOR") 3 1 0 0 * 0.35

-- MiRings ; 5=STRING_AND_REVERB --> spacey ; force 'internal exciter'
let md = X.miRings_mode "STRING_AND_REVERB"
    input = pinkNoise 'α' AR * 0.2
    tr = dust 'β' KR 0.7
    struct = range 0 1 (lfNoise1 'γ' KR 0.4)
    pos = range 0 1 (lfNoise2 'δ' KR 0.1)
in X.miRings AR input tr 60 struct 0.5 0.7 pos md 4 1 0 0 * 0.25

-- MiRings ; easter egg --> drone ; set damp to 1.0 to get a sustaining sound that doesn't need to be triggered
let struct = range 0 1 (lfNoise2 'α' KR 0.2)
    brightness = range 0 1 (lfNoise1 'β' KR 0.5)
in X.miRings AR 0 0 40 struct brightness 1 0.9 (X.miRings_mode "MODULATED/INHARMONIC_STRING") 2 0 1 0 * 0.25
