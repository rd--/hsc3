-- latch ; sample & hold
let n = whiteNoiseId 'α' ar
    i = impulse ar 9 0
    l = latch n i
in blip ar (l * 400 + 500) 4 * 0.2

-- latch ; c.f. LFNoise0
let n1 = latch (whiteNoiseId 'α' ar) (impulse ar 9 0)
    n2 = lfNoise0Id 'α' kr 9
in blip ar (mce2 n1 n2 * 400 + 500) 4 * 0.2

-- latch ; http://create.ucsb.edu/pipermail/sc-users/2006-December/029991.html
let n0 = lfNoise2Id 'α' kr 8
    n1 = lfNoise2Id 'β' kr 3
    s = blip ar (n0 * 200 + 300) (n1 * 10 + 20)
    x = mouseX kr 1000 (sampleRate * 0.1) Exponential 0.1
in latch s (impulse ar x 0)

-- latch ; density control for sample and hold of noise signal (c.f. dust)
let n0 = whiteNoiseId 'α' ar
    n1 = whiteNoiseId 'β' ar
    k = 1000
    c = control_m kr "density" 26 (0.01,k,"exp")
in latch n0 (n1 `greater_than` ((constant k - c) / constant k)) * 0.1

-- latch ; density control for sample and hold of noise signal
let n = whiteNoiseId 'α' ar
    c = control_m kr "density" 26 (0.01,8000,"exp")
in latch n (dustId 'γ' ar c) * 0.1

---- ; drawings
UI.ui_sc3_scope 2 0 4096 1 "audio" 0
