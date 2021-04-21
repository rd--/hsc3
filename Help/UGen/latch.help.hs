-- latch ; sample & hold
let n = whiteNoise 'α' AR
    i = impulse AR 9 0
    l = latch n i
in blip AR (l * 400 + 500) 4 * 0.2

-- latch ; c.f. LFNoise0
let n1 = latch (whiteNoise 'α' AR) (impulse AR 9 0)
    n2 = lfNoise0 'α' KR 9
in blip AR (mce2 n1 n2 * 400 + 500) 4 * 0.2

-- latch ; http://create.ucsb.edu/pipermail/sc-users/2006-December/029991.html
let n0 = lfNoise2 'α' KR 8
    n1 = lfNoise2 'β' KR 3
    s = blip AR (n0 * 200 + 300) (n1 * 10 + 20)
    x = mouseX KR 1000 (sampleRate * 0.1) Exponential 0.1
in latch s (impulse AR x 0)

-- latch ; density control for sample and hold of noise signal (c.f. dust)
let n0 = whiteNoise 'α' AR
    n1 = whiteNoise 'β' AR
    k = 1000
    c = control_m KR "density" 26 (0.01,k,"exp")
in latch n0 (n1 `greater_than` ((constant k - c) / constant k)) * 0.1

-- latch ; density control for sample and hold of noise signal
let n = whiteNoise 'α' AR
    c = control_m KR "density" 26 (0.01,8000,"exp")
in latch n (dust 'γ' AR c) * 0.1

---- ; drawings
UI.ui_sc3_scope 2 0 4096 1 "audio" 0
