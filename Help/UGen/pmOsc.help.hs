-- pmOsc ; composite of sinOsc, ie. sinOsc r cf (sinOsc r mf mp * pm) ; modulate carfreq
pmOsc ar (line kr 600 900 5 DoNothing) 600 3 0 * 0.1

-- pmOsc ; modulate modfreq
pmOsc ar 300 (line kr 600 900 5 DoNothing) 3 0 * 0.1

-- pmOsc ; modulate index
pmOsc ar 300 550 (line kr 0 20 8 DoNothing) 0 * 0.1

-- pmOsc ; random parameters, linear modulation index motion over n seconds
let n = 2
    cf = randId 'α' 0 2000
    mf = randId 'β' 0 800
    pme = randId 'γ' 0 12
    l = randId 'δ' (-1) 1
    pm = line kr 0 pme n DoNothing
in linPan2 (pmOsc ar cf mf pm 0) l 0.05

-- pmOsc ; event control
let f (_,g,x,y,z,_,_,_,_,_,_) =
      let cps = midiCps (x * 24 + 42)
          sig = pmOsc ar
                (vibratoId 'θ' ar (k2a cps) (y * 4 + 4) 0.02 0 0 0.04 0.1 0.0 0.0)
                (urange 1.01 2.01 (lfPulse kr (1/8) 0 0.5) * cps)
                (envGen kr g 1 0 1 DoNothing (envelope [3,3,0] [0, 0.2] [EnvNum (-4)]))
                0
      in mix (sig * ampCompA kr cps 0 0.32 1 * z * g)
    rvb s = xFade2 s (gVerb (bpf s (midiCps 90) 1) 50 8 0.5 0.5 15 0 0.7 0.5 300) 0.2 1
in rvb (mix (eventVoicer 16 f)) * control kr "gain" 1
