-- stkMoog ; mouse trigger
let tr = mouseButton KR 0 1 0 - 0.5
    freq = midiCPS (tiRand 'α' 12 96 tr)
    filterQ = tRand 'β' 0 127 tr
    sweeprate = tRand 'γ' 0 127 tr
    vibfreq = tRand 'δ' 0 127 tr
    vibgain = tRand 'ε' 0 127 tr
    gain = tRand 'ζ' 0 127 tr
in X.stkMoog AR freq filterQ sweeprate vibfreq vibgain gain tr * 0.5

-- stkMoog ; mouse control
let x = mouseX KR 0.25 16 Linear 0.2
    tr = impulse KR x 0 - 0.5
    freq = midiCPS (tiRand 'α' 24 72 tr)
    filterQ = tRand 'β' 0 32 tr
    sweeprate = tRand 'γ' 0 32 tr
    vibfreq = tRand 'δ' 0 96 tr
    vibgain = tRand 'ε' 0 16 tr
    gain = tRand 'ζ' 0 127 tr
in X.stkMoog AR freq filterQ sweeprate vibfreq vibgain gain tr * 0.5

-- stkMoog ; sequence
let nsig z l r f = range l r (lfNoise2 z KR f)
    scl = asLocalBuf 'α' [0,2,3.2,5,7,9,10]
    mnn = 48 + degreeToKey scl (nsig 'β' 0 15 0.35) 12
    freq = midiCPS mnn
    filterQ = nsig 'γ' 0 64 0.5
    sweeprate = nsig 'δ' 0 64 0.5
    vibfreq = nsig 'ε' 0 64 0.5
    vibgain = nsig 'ζ' 0 16 0.5
    gain = nsig 'η' 16 96 0.5
in X.stkMoog AR freq filterQ sweeprate vibfreq vibgain gain 1

-- stkMoog ; event control
let f _ (g,_,y,z,o,rx,ry,p) =
      let freq = midiCPS p
          filterQ = y * 128
          sweeprate = rx * 16
          vibfreq = y * 64
          vibgain = ry * 4
          gain = linLin z 0 1 16 96
          sig = X.stkMoog AR freq filterQ sweeprate vibfreq vibgain gain 1
      in pan2 sig (o * 2 - 1) (lagUD g 0.01 0.25)
in mix (rEventVoicer 16 f) * control KR "gain" 1
