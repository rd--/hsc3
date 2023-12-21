-- MiPlaits ; basics ; 0=virtual_analog
X.miPlaits ar 35 (X.miPlaits_mode "virtual_analog") 0.252 0.5 0.2 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; Mouse control ; 2=fm
let harm = mouseX kr 0 1 Linear 0.2
    timb = mouseY kr 0 1 Linear 0.2
in X.miPlaits ar 48 (X.miPlaits_mode "fm") harm timb 0.5 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; some talking ; 7=speech
let ptch = mouseY kr 24 72 Linear 0.2
    harm = mouseX kr 0 1 Linear 0.2
    mrph = lfSaw ar 0.11 0 * 0.5 + 0.5
in X.miPlaits ar ptch (X.miPlaits_mode "speech") harm 0.5 mrph 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; scanning through some engines + vibrato
let pit = 50
    vib = sinOsc ar 6 0 * 0.3
    eng = lfNoise0Id 'α' kr 0.5 * 4 + 4
in X.miPlaits ar (pit + vib) eng 0.1 0.5 0.5 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; some FM
let harm = mouseX kr 0 1 Linear 0.2
    timbre = mouseY kr 0 1 Linear 0.2
    morph = range 0 1 (lfNoise2Id 'α' kr 0.2)
    trigger = impulse ar 8 0
in X.miPlaits ar 48 (X.miPlaits_mode "fm") harm timbre morph trigger 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; 5=wavetable
let trigger = impulse kr 8 0
    harm = lfNoise2Id 'α' kr 0.3 * 0.25 + 0.25
    timbre = lfNoise1Id 'α' kr 0.4 * 0.5 + 0.5
    dec = range 0.01 0.8 (lfNoise2Id 'α' kr 0.2)
in X.miPlaits ar 44 (X.miPlaits_mode "wavetable") harm timbre 0 trigger 0 0 0 0 dec 0.5 * 0.2

-- MiPlaits ; 6=chord
let harm = mouseY kr 0 1 Linear 0.2 -- select chord
    trigger = impulse kr 6 0
    lev = tRandId 'α' 0 1 trigger
in mceChannel 0 (X.miPlaits ar 40 (X.miPlaits_mode "chord") harm 0.5 0.5 trigger (lev * lev) 0 0.4 0 0.5 0.5) * 0.1

-- MiPlaits ; 8=swarm ; super saw
let pit = tiRandId 'α' 39 49 (dustId 'β' kr 0.5)
in mceChannel 0 (X.miPlaits ar pit (X.miPlaits_mode "swarm") 0.16 0 1 0 0 0 0 0 0.5 0.5) * 0.1

-- MiPlaits ; slow swarm
X.miPlaits ar 48 (X.miPlaits_mode "swarm") 0.9 0 0.5 0 0 0 0 0 0.5 0.5 * mce2 0.04 0.15

-- MiPlaits ; 11=string ; inharmonic
let harm = range 0 1 (lfNoise2Id 'α' kr 0.2)
in mceChannel 0 (X.miPlaits ar 60 (X.miPlaits_mode "string") harm 0 0.85 0 0 0 0 0 0.5 0.5)

-- MiPlaits ; 12=modal
let harm = range 0 1 (lfNoise2Id 'α' kr 0.2)
    trigger = impulse kr 1 0
    lev = squared (tRandId 'β' 0 1 trigger)
in mix (X.miPlaits ar 56 (X.miPlaits_mode "modal") harm 0 0.85 trigger lev 0 0 0 0.5 0.5)

-- MiPlaits ; snare drums
let harm = range 0 0.5 (lfNoise2Id 'α' kr 0.5)
    timbre = 0.173
    morph = 0.827
    trigger = impulse kr 8 0
    lev = squared (tRandId 'β' 0 1 trigger)
in X.miPlaits ar 47 14 harm timbre morph trigger lev 0.2 0.43 0 0.4 0.5 * 0.1

-- MiPlaits ; filtered noise
let harm = 0.575
    timbre = 0.02
    morph = range_hs (0.55,0.9) (lfNoise1Id 'α' kr 0.3) -- filter resonance
in X.miPlaits ar 62 9 harm timbre morph 0 0 0 0 0 0.5 0.5 * 0.25

-- MiPlaits ; something...
let trigger = dustId 'α' kr (range_hs (0.1,7) (lfNoise2Id 'β' kr 0.1))
    harmonics = sinOsc kr 0.03 0 * 0.5 + 0.5
    timbre = lfTri kr 0.07 0 * 0.5 + 0.5
    morph = lfTri kr 0.11 0 * 0.5 + 0.5
    pitch_ = tiRandId 'γ' 24 48 trigger
    engine = roundE (tRandId 'δ' 0 15 trigger)
    sub = sinOsc ar (midiCps pitch_) 0 * 0.1
    mi = X.miPlaits ar pitch_ engine harmonics timbre morph trigger 0 0 0 0 0.8 0.2 * 0.5
in (mi + sub) * 0.2

