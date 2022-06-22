-- lagUD ; lag pitch, slower down (5 seconds) than up (1 second)
let x = mouseX kr 220 440 Linear 0.2
in sinOsc ar (mce2 x (lagUD x 1 5)) 0 * 0.1

-- lagUD ; as signal filter
let x = mouseX kr 0.0001 0.01 Exponential 0.2
    y = mouseY kr 0.0001 0.01 Exponential 0.2
in lagUD (0 - saw ar 440) x y * 0.15

-- lagUD ; as signal filter
let x = mouseX kr 0.0001 0.01 Exponential 0.2
    y = mouseY kr 0.0001 0.01 Exponential 0.2
in lagUD (impulse ar (range 6 24 (lfNoise2Id 'α' kr 4)) 0) x y * 0.5

-- lagUD ; as signal filter
let x = mouseX kr 0.0001 0.01 Exponential 0.2
    y = mouseY kr 0.0001 0.01 Exponential 0.2
in lagUD (lfPulse ar 800 0 0.5 * 2 - 1) x y * 0.05

-- lagUD ; as signal filter
let x = mouseX kr 0.0001 0.01 Exponential 0.2
    y = mouseY kr 0.0001 0.01 Exponential 0.2
in lagUD (varSaw ar 220 0 (range 0 1 (sinOsc kr 0.25 0))) x y * 0.1

-- lagUD
let x = mouseX kr 0.0 (1/100) Linear 0.2
    y = mouseY kr 0.0 (3/100) Linear 0.2
in lagUD (lfPulse ar 50 0 0.25) x y * 0.1
