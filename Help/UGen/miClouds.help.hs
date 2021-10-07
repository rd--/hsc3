-- MiClouds ; basic granulation ; requires=buf (stereo) ; 0=GRANULAR
let buf = control kr "buf" 0
    nc = 2
    input = playBuf nc ar buf 1 1 0 Loop DoNothing
    dens = lfNoise1Id 'α' kr 0.3 `in_range` (0.3,0.45)
in X.miClouds ar 0 0 0 dens 0.5 1 1 0.5 0 0 0 (X.miClouds_mode "GRANULAR") 0 0 input

-- MiClouds
let imp = dustId 'α' ar (mce2 0.8 1.1)
    freq = midiCps (latch (pinkNoiseId 'β' ar * 24 + 80) imp)
    input = rlpf imp freq 0.002 * 4
    pit = lfNoise1Id 'γ' kr 0.3 * 12
    pos = lfNoise2Id 'δ' kr 0.4 * 0.5 + 0.5
    size = lfNoise1Id 'ε' kr 0.3 * 0.5 + 0.5
    dens = lfNoise1Id 'ζ' kr 0.3 * 0.5 + 0.5
    tex = lfNoise1Id 'η' kr 0.3 * 0.5 + 0.5
    frez = lfClipNoiseId 'θ' kr 0.3
in X.miClouds ar pit pos size dens tex 0.5 2 0.5 0.3 0.8 frez (X.miClouds_mode "GRANULAR") 1 0 input

-- MiClouds ; using external grain trigger ; requires=buf (stereo)
let buf = control kr "buf" 0
    nc = 2
    input = playBuf nc ar buf 1 1 0 Loop DoNothing
    dens = 0.5
    tr = dustId 'α' kr 10
in X.miClouds ar (-5) 0 0.2 dens 0.5 1 1 0.5 0 0 0 (X.miClouds_mode "GRANULAR") 0 tr input

-- MiClouds ; 3=SPECTRAL ; (this mode causes high CPU peaks)
let buf = control kr "buf" 0
    nc = 2
    input = playBuf nc ar buf 1 1 0 Loop DoNothing
    size = 0.35
    dens = 0.02
    tex = 0.3
in X.miClouds ar 0 0 size dens tex 1 1 0.5 0.8 0.8 0 (X.miClouds_mode "SPECTRAL") 0 0 input * 0.35

-- MiClouds ; spectral again ; watch volume
let buf = control kr "buf" 0
    nc = 2
    input = playBuf nc ar buf 1 1 0 Loop DoNothing
    size = squared (range 0.1 0.5 (lfNoise1Id 'α' kr 0.2))
    dens = lfNoise1Id 'β' kr 0.3 * 0.5 + 0.5
    tex = lfNoise1Id 'γ' kr 0.3 * 0.5 + 0.5
in X.miClouds ar 0 0 size dens tex 1 1 0.5 0.6 0.6 0 (X.miClouds_mode "SPECTRAL") 1 0 input * 0.25

---- ; allocate buffer 0, required for examples
ld fn = withSC3 (async (b_allocRead 0 (sfRequire fn) 0 0))
ld "crotale05(D).wav"
ld "saron-panerus-S-0-5.flac"
