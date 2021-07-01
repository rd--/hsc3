-- pow
let a = fSinOsc ar 100 0 * 0.1 in mce2 a (a ** 10)

-- pow ; see also http://create.ucsb.edu/pipermail/sc-users/2006-December/029998.html
let n0 = lfNoise2Id 'α' kr 8
    n1 = lfNoise2Id 'β' kr 3
    s = blip ar (n0 * 200 + 300) (n1 * 10 + 20)
    x = mouseX kr 1000 (sampleRate * 0.5) Exponential 0.1
    y = mouseY kr 1 24 Exponential 0.1
    d = latch s (impulse ar x 0)
    b = roundUp d (0.5 ** y)
in mce2 d b * 0.2

-- pow ; optimises identity
(sinOsc ar 440 0 ** 1) * 0.1
