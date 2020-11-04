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
