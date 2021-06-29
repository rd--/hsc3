-- MiVerb ; basic
X.miVerb 0.7 0.5 0.5 0.05 0 0.625 (impulse ar 1 0)

-- MiVerb ; stereo ; with freezing
let imp = dust 'α' ar (mce2 0.7 0.8)
    freq = midiCPS (latch (pinkNoise 'β' ar * 24 + 80) imp)
    input = rlpf imp freq 0.002 * 3
    freez = tRand 'γ' (-1) 1 (dust 'δ' kr 0.7)
    revtime = 0.8
    drywet = 0.5
    damping = 0.3
    diff = lfNoise1 'ε' kr 0.1 * 0.5 + 0.5
in X.miVerb revtime drywet damping 0.05 freez 0.625 input

-- MiVerb ; reverb time (> 1)
let freq = lfNoise0 'α' kr 0.3 `in_range` (400, 2500)
    input = rlpf (impulse ar 0.3 0) freq 0.1
    time = lfNoise2 'β' kr 0.3 * 0.1 + 1.03 -- modulate rev time above and below 1.0!
    damp = lfNoise2 'γ' kr 0.2 `in_range` (0, 0.7)
in X.miVerb time 0.9 damp 0.1 0 0.625 input * 0.25
