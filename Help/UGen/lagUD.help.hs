-- lagUD ; lag pitch, slower down (5 seconds) than up (1 second)
let x = mouseX KR 220 440 Linear 0.2
in sinOsc AR (mce2 x (lagUD x 1 5)) 0 * 0.1

-- lagUD ; as signal filter
let x = mouseX KR 0.0001 0.01 Exponential 0.2
    y = mouseY KR 0.0001 0.01 Exponential 0.2
in lagUD (0 - saw AR 440) x y * 0.15

-- lagUD ; as signal filter
let x = mouseX KR 0.0001 0.01 Exponential 0.2
    y = mouseY KR 0.0001 0.01 Exponential 0.2
in lagUD (impulse AR (range 6 24 (lfNoise2 'α' KR 4)) 0) x y * 0.5

-- lagUD ; as signal filter
let x = mouseX KR 0.0001 0.01 Exponential 0.2
    y = mouseY KR 0.0001 0.01 Exponential 0.2
in lagUD (lfPulse AR 800 0 0.5 * 2 - 1) x y * 0.05

-- lagUD ; as signal filter
let x = mouseX KR 0.0001 0.01 Exponential 0.2
    y = mouseY KR 0.0001 0.01 Exponential 0.2
in lagUD (varSaw AR 220 0 (range 0 1 (sinOsc KR 0.25 0))) x y * 0.1

-- lagUD
let x = mouseX KR 0.0 (1/100) Linear 0.2
    y = mouseY KR 0.0 (3/100) Linear 0.2
in lagUD (lfPulse AR 50 0 0.25) x y * 0.1
