-- zeroCrossing
let a = sinOsc AR (sinOsc KR 1 0 * 600 + 700) 0 * 0.1
in mce [a, impulse AR (zeroCrossing a) 0 * 0.25]

-- zeroCrossing
let a = soundIn 0
in mce [a, sinOsc AR (zeroCrossing a) 0 * 0.1]
