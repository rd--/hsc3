-- MiClouds ; basic granulation ; requires=buf (stereo) ; 0=GRANULAR
let buf = control KR "buf" 0
    nc = 2
    input = playBuf nc AR buf 1 1 0 Loop DoNothing
    dens = range 0.3 0.45 (lfNoise1 'α' KR 0.3)
in X.miClouds AR 0 0 0 dens 0.5 1 1 0.5 0 0 0 (X.miClouds_mode "GRANULAR") 0 0 input

-- MiClouds
let imp = dust 'α' AR (mce2 0.8 1.1)
    freq = midiCPS (latch (pinkNoise 'β' AR * 24 + 80) imp)
    input = rlpf imp freq 0.002 * 4
    pit = lfNoise1 'γ' KR 0.3 * 12
    pos = lfNoise2 'δ' KR 0.4 * 0.5 + 0.5
    size = lfNoise1 'ε' KR 0.3 * 0.5 + 0.5
    dens = lfNoise1 'ζ' KR 0.3 * 0.5 + 0.5
    tex = lfNoise1 'η' KR 0.3 * 0.5 + 0.5
    frez = lfClipNoise 'θ' KR 0.3
in X.miClouds AR pit pos size dens tex 0.5 2 0.5 0.3 0.8 frez (X.miClouds_mode "GRANULAR") 1 0 input

-- MiClouds ; using external grain trigger
let buf = control KR "buf" 0
    nc = 2
    input = playBuf nc AR buf 1 1 0 Loop DoNothing
    dens = 0.5
    tr = dust 'α' KR 10
in X.miClouds AR (-5) 0 0.2 dens 0.5 1 1 0.5 0 0 0 (X.miClouds_mode "GRANULAR") 0 tr input

-- MiClouds ; 3=SPECTRAL ; (this mode causes high CPU peaks)
let buf = control KR "buf" 0
    nc = 2
    input = playBuf nc AR buf 1 1 0 Loop DoNothing
    size = 0.35
    dens = 0.02
    tex = 0.3
in X.miClouds AR 0 0 size dens tex 1 1 0.5 0.8 0.8 0 (X.miClouds_mode "SPECTRAL") 0 0 input * 0.35

-- MiClouds ; spectral again ; watch volume
let buf = control KR "buf" 0
    nc = 2
    input = playBuf nc AR buf 1 1 0 Loop DoNothing
    size = squared (range 0.1 0.5 (lfNoise1 'α' KR 0.2))
    dens = lfNoise1 'β' KR 0.3 * 0.5 + 0.5
    tex = lfNoise1 'γ' KR 0.3 * 0.5 + 0.5
in X.miClouds AR 0 0 size dens tex 1 1 0.5 0.6 0.6 0 (X.miClouds_mode "SPECTRAL") 1 0 input * 0.25
