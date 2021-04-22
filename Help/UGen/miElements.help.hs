-- miElements ; basic ; model=0=Modal
let inp = pinkNoise 'α' AR * 0.3
    gat = lfPulse KR 2 0 0.5
in X.miElements AR {-blow_in-} inp 0 gat 45 0.5 0.2 0 0 0 0.5 0.5 0.5 0.5 0.5 0.25 0.5 0.7 0.2 0.3 {-model-} 0 0

-- miElements ; ringing ; use 'blow' input and contour set to 0.5
let inp = pinkNoise 'α' AR * 0.3
in X.miElements AR inp 0 1 48 0.5 {-contour-} 0.5 0 0 0 0.5 0.5 0.5 0.5 0.5 0.25 0.5 0.7 0.2 0.3 0 0

-- miElements ; ringing ; use the 'strike' input (which bypasses the exciter section)
let inp = pinkNoise 'α' AR * 0.3
in X.miElements AR 0 {-strike_in-} inp 0 48 0.5 0.2 0 0 0 0.5 0.5 0.5 0.5 0.5 0.25 0.5 0.7 0.2 0.3 0 0

-- miElements ; bow
let pit = iRand 'α' 32 44
    bow_timb = lfNoise1 'β' KR 0.3 * 0.5 + 0.5
in X.miElements AR 0 0 1 pit 0.5 0.5 1 0 0 0.5 0.5 bow_timb 0.5 0.5 0.25 0.5 0.7 0.2 0.3 0 0 * 0.25

-- miElements ; blow
let mod1 = lfNoise1 'α' KR 0.4 * 0.5 + 0.5
    mod2 = lfNoise1 'β' KR 0.2 * 0.5 + 0.5
    pit = lfNoise0 'γ' KR 0.1 `in_range` (32,44)
in X.miElements AR 0 0 1 pit 0.5 0.5 0 0.6 0 mod1 0.5 0.5 mod2 0.5 0.25 0.5 0.7 0.2 0.3 0 0 * 0.2

-- miElements ; blow ; contour
let gat = lfPulse KR 1 0.01 0.5
    pit = sinOsc KR 5 0 * 0.1 + 53
    cont = sinOsc KR 0.8 0 `in_range` (0,1)
    flow = lfNoise1 'α' KR 0.1 `in_range` (0,1)
in X.miElements AR 0 0 gat pit 0.5 cont 0 0.5 0 flow 0.5 0.5 0.3 0.5 0.25 0.3 0.8 0.2 0.3 0 0 * 0.25

-- miElements ; model=0=MODAL ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let tr = trig1 g controlDur
          pit = x * 24 + 56
          stre = ry * 0.5 + 0.25
          cont = y
          flo = o
          geo = 0.15 + (latch y tr * 0.35)
          bri = 0.5 - y * 0.35
          dmp = 1 - x * 0.25
          pos = o * 0.5
          spc = 0.5 - y * 0.5
          md = 0
      in X.miElements AR 0 0 g pit stre cont 0 z 0 flo 0.5 0.5 (0.25 + rx * 0.5) 0.5 geo bri dmp pos spc md 0 * 0.25
in mix (rEventVoicer 6 f) * control KR "gain" 1

-- miElements ; metal, bells
let tr = dust 'α' AR 2.5
    inp = decay tr 0.01
    g = X.tBrownRand 'β' 0.5 0.9 0.2 0 (coinGate 'γ' 0.05 tr)
    space = range 0.5 1 (lfNoise1 'δ' KR 0.1)
in X.miElements AR 0 inp 0 40 0.5 0.2 0 0 0 0.5 0.5 0.5 0.5 0.5 g 0.4 0.9 0.2 space 0 0 * 0.35

-- miElements ; metal, bells ; event control
let f _ (g,x,y,z,o,_,_,_,_,_) =
      let tr = trig1 (k2a g) sampleDur
          inp = decay (tr * z * 4) 0.01
          pit = latch x g * 12 + 36
          geo = latch y g * 0.4 + 0.5
          spc = o * 0.5 + 0.5
      in X.miElements AR 0 inp 0 pit 0.5 0.2 0 0 0 0.5 0.5 0.5 0.5 0.5 geo 0.4 0.9 0.2 spc 0 0
in mix (rEventVoicer 6 f) * control KR "gain" 1

-- miElements ; strike input ; playing chords ; model=2=Strings
let inp = decay (dust 'α' AR 1) 0.01
    g = lfNoise1 'β' KR 0.1 * 0.5 + 0.5
in X.miElements AR 0 inp 0 {-pit-} 53 0.5 0.2 0 0 0 0.5 0.5 0.5 0.5 0.5 {-geom-} g {-bright-} 0.5 {-damp-} 0.9 0.2 0.3 {-model-} 2 0

-- miElements ; metal, bells ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let tr = trig1 (k2a g) sampleDur
          inp = decay tr 0.01
          pit = x * 12 + 48
          geo = latch o g -- lfNoise1 'β' KR 0.1 * 0.5 + 0.5
          bri = latch y g
          dam = latch (0.9 - z) g
          md = 2
      in X.miElements AR 0 inp 0 pit 0.5 0.2 0 0 0 0.5 0.5 0.5 0.5 0.5 geo bri dam 0.2 0.3 md 0
in mix (rEventVoicer 6 f) * control KR "gain" 1

-- miElements ; mallets, strength
let gat = coinGate 'α' 0.4 (impulse KR 6 0)
    stren = tRand 'β' 0 1 gat
    strike_timbre = lfNoise1 'γ' KR 0.3 * 0.5 + 0.5
in X.miElements AR 0 0 gat {-pit-} 40 stren 0.2 0 0 {-strike_level-} 0.5 0.5 {-mallet-} 0.7 0.5 0.5 strike_timbre 0.25 {-bright-} 0.3 {-damp-} 0.85 0.2 {-space-} 0.6 0 0 * 0.5

-- miElements ; mallets ; particles (mallet type=1 --> use internal model of bouncing particles)
let strike_timbre = lfNoise1 'α' KR 0.3 * 0.5 + 0.5
    g = range 0.4 0.7 (lfNoise2 'β' KR 0.1)
    maltype = 1
in X.miElements AR 0 0 {-gate-} 1 {-pit-} 40 0.5 {-contour-} 0.5 0 0 {-strike_level-} 0.5 0.5 {-mallet-} maltype 0.5 0.5 strike_timbre {-geom-} g 0.5 0.7 0.2 0.3 0 0

-- miElements ; easteregg: 2x2-op FM synth
let n = lfNoise1 'α' KR 0.3 * 0.5 + 0.5
    (r1,r2,r3) = (0.25,0.25,0.51)
in X.miElements AR 0 0 {-gate-} 1 48 {-strength-} 0.9 {-contour-} 0.5 {-bow_level-} r1 {-blow_level-} 0.6 {-strike_level-} 0.5 {-flow-} r2 {-mallet-} r3 {-bow_timb-} n {-blow_timb-} 0.3 {-strike_timb-} 0.6  {-geom-} 0.12 {-bright-} 0.6 {-damp-} 0.5 0.2 {-space-} 0.7 0 {-easteregg-} 1
