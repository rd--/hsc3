-- lag ; mouse
let x = mouseX kr 220 440 Linear 0.2
in sinOsc ar (mce [x, lag x 1]) 0 * 0.1

-- lag ; noise
let n = lfNoise0Id 'α' kr 0.5
in sinOsc ar (220 + (lag n 1 * 220)) 0 * (lag n 2 * 0.1)

-- lag
lag (impulse ar 100 0) (mouseX kr 0.0 0.01 Linear 0.2)

-- lag
lag (lfPulse ar 50 0 0.5) (mouseX kr 0.0 (1/50) Linear 0.2) * 0.2

-- lag ; frequency plain at left and smoothed at right
let f = sinOsc kr 0.05 0.0 `in_range` (220,440)
in sinOsc ar (mce2 f (lag f 1)) 0 * 0.1
