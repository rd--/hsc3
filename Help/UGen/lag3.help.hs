-- lag3 ; mouse control
let x = mouseX KR 220 440 Exponential 0.1
in sinOsc AR (mce [x, lag3 x 1]) 0 * 0.1

-- lag3 ; audio
lag3 (impulse AR 100 0) (mouseX KR 0.0 0.01 Linear 0.2)

-- lag3
lag3 (lfPulse AR 100 0 0.5 * 0.2) (mouseX KR 0.0 0.01 Linear 0.2)

-- lag3 ; written out
let x = mouseX KR 0.0 0.01 Linear 0.2
in lag (lag (lag (lfPulse AR 100 0 0.5 * 0.2) x) x) x
