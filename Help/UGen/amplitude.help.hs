-- amplitude ; control gain
let s = soundIn 0
    a = amplitude KR s 0.01 0.01
in pulse AR 90 0.3 * a

-- amplitude ; control frequency
let s = soundIn 0
    f = amplitude KR s 0.5 0.5 * 1200 + 400
in sinOsc AR f 0 * 0.1

-- amplitude
let s = soundIn 0
    a = amplitude AR s 0.5 0.05
in s * a
