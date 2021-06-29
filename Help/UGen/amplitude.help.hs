-- amplitude ; control gain
let s = soundIn 0
    a = amplitude kr s 0.01 0.01
in pulse ar 90 0.3 * a

-- amplitude ; control frequency
let s = soundIn 0
    f = amplitude kr s 0.5 0.5 * 1200 + 400
in sinOsc ar f 0 * 0.1

-- amplitude
let s = soundIn 0
    a = amplitude ar s 0.5 0.05
in s * a
