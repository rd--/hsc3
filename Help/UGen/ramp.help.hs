-- ramp ; lag pitch
let o = lfPulse KR 4 0 0.5 * 50 + 400
    l = line KR 0 1 15 DoNothing
    f = ramp o l
in sinOsc AR f 0 * 0.3

-- ramp ; mouse control
let x1 = mouseX KR 220 440 Exponential 0
    x2 = ramp x1 (300 / 1000)
in sinOsc AR (mce2 x1 x2) 0 * 0.1
