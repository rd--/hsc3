-- lag ; mouse
let x = mouseX KR 220 440 Linear 0.2
in sinOsc AR (mce [x, lag x 1]) 0 * 0.1

-- lag ; noise
let n = lfNoise0 'α' KR 0.5
in sinOsc AR (220 + (lag n 1 * 220)) 0 * (lag n 2 * 0.1)

-- lag
lag (impulse AR 100 0) (mouseX KR 0.0 0.01 Linear 0.2)

-- lag
lag (lfPulse AR 50 0 0.5) (mouseX KR 0.0 (1/50) Linear 0.2) * 0.2

-- lag ; frequency plain at left and smoothed at right
let f = sinOsc KR 0.05 0.0 `in_range` (220,440)
in sinOsc AR (mce2 f (lag f 1)) 0 * 0.1
