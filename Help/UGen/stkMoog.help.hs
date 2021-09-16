-- stkMoog ; mouse trigger
let tr = mouseButton kr 0 1 0 - 0.5
    freq = midiCps (tiRandId 'α' 12 96 tr)
    filterQ = tRandId 'β' 0 127 tr
    sweeprate = tRandId 'γ' 0 127 tr
    vibfreq = tRandId 'δ' 0 127 tr
    vibgain = tRandId 'ε' 0 127 tr
    gain = tRandId 'ζ' 0 127 tr
in X.stkMoog ar freq filterQ sweeprate vibfreq vibgain gain tr * 0.5

-- stkMoog ; mouse control
let x = mouseX kr 0.25 16 Linear 0.2
    tr = impulse kr x 0 - 0.5
    freq = midiCps (tiRandId 'α' 24 72 tr)
    filterQ = tRandId 'β' 0 32 tr
    sweeprate = tRandId 'γ' 0 32 tr
    vibfreq = tRandId 'δ' 0 96 tr
    vibgain = tRandId 'ε' 0 16 tr
    gain = tRandId 'ζ' 0 127 tr
in X.stkMoog ar freq filterQ sweeprate vibfreq vibgain gain tr * 0.5

-- stkMoog ; sequence
let nsigId z l r f = range l r (lfNoise2Id z kr f)
    scl = asLocalBufId 'α' [0,2,3.2,5,7,9,10]
    mnn = 48 + degreeToKey scl (nsigId 'β' 0 15 0.35) 12
    freq = midiCps mnn
    filterQ = nsigId 'γ' 0 64 0.5
    sweeprate = nsigId 'δ' 0 64 0.5
    vibfreq = nsigId 'ε' 0 64 0.5
    vibgain = nsigId 'ζ' 0 16 0.5
    gain = nsigId 'η' 16 96 0.5
in X.stkMoog ar freq filterQ sweeprate vibfreq vibgain gain 1

-- stkMoog ; event control
let f _ (g,_,y,z,o,rx,ry,p,px,_) =
      let freq = midiCps (p + px)
          filterQ = y * 128
          sweeprate = rx * 16
          vibfreq = y * 64
          vibgain = ry * 4
          gain = linLin z 0 1 16 96
          sig = X.stkMoog ar freq filterQ sweeprate vibfreq vibgain gain 1
      in pan2 sig (o * 2 - 1) (lagUD g 0.01 0.25)
in mix (eventVoicer 16 f) * control kr "gain" 1
